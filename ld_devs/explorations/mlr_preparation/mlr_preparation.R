#'---
#'author: "Thomas Goossens (CRA-W) - t.goossens@cra.wallonie.be"
#'output: 
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'title: "R script to retrieve Agromet API data"
#'date: Sys.Date()
#'---
source("./R/file_management.R")
source_files_recursively.fun("./R")
source_files_recursively.fun("./ld_devs/explorations/mlr_preparation/R")

#+ ---------------------------------
#' ## Data acquisition
#' 
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#' ### Dependent variables

# static variables
expl.static.grid.df



# Retrieving from API
dynamic.records.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "all",
    sensors.chr = "tsa,ens",
    dfrom.chr = "2018-06-11",
    dto.chr = "2018-06-12",
    api_v.chr = "v2"
  ), "cleandata"
)

# Filtering records to keep only the useful ones
dynamic.records.df <- dynamic.records.df %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(!is.na(to)) %>%
  filter(state == "Ok") %>%
  filter(!is.na(tsa))

# Selecting only the useful features
dynamic.records.df <- dynamic.records.df %>%
  dplyr::select(one_of(c("sid", "mtime", "longitude", "latitude", "altitude", "tsa" ,"ens")))

#+ ---------------------------------
#' ## mlr benchmark experiment preparation
#' 
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# defining the target var
target.chr = "tsa"

# defining the learners who will be compared
lrns.l <- list(
  makeFilterWrapper(
    learner = makeLearner(cl = "regr.lm", id="linear regression"),  fw.method = "information.gain", fw.abs = 2),
  # makeLearner(cl = "regr.lm", id="linear regression"),
  # makeLearner(cl = "regr.elmNN", id="single layer neural net"),
  # makeLearner(cl ="regr.kknn", id="nearest neighbours"),
  makeLearner(cl = "regr.km", id="kriging")
)

# defining the validation (resampling) strategy
resampling.l = mlr::makeResampleDesc(
  method = "LOO"#,
  #predict = "test"
)

# Building a nested data frame, where for each hourly observation we have a 30 stations dataset of 1h temperature record.
library(purrr)
stations.nested.df <- dynamic.records.df %>%
  group_by(mtime) %>%
  tidyr::nest()

# converting each tibble of the nested records to a strict dataframe (required by mlr)
# ::todo:: need to use transmute_at
stations.nested.df <- stations.nested.df %>%
  mutate(data_as_df = purrr::map(
    .x = data,
    .f = data.frame
  ))

# defining the regression tasks on the stations observations for each of the hourly datasets
# https://stackoverflow.com/questions/46868706/failed-to-use-map2-with-mutate-with-purrr-and-dplyr
# https://stackoverflow.com/questions/42518156/use-purrrmap-to-apply-multiple-arguments-to-a-function?rq=1
stations.nested.df <- stations.nested.df %>%
  mutate(all_vars.stations.task = purrr::map2(
    as.character(mtime),
    data_as_df,
    mlr::makeRegrTask,
    target = target.chr
  )
  )

stations.nested.df <- stations.nested.df %>%
  mutate(all_vars.stations.task2 = purrr::map2(
    all_vars.stations.task,
    "sid",
    mlr::dropFeatures
  ))

# fusing various tasks to the learners in the most convenient way
# https://mlr-org.github.io/mlr/articles/tutorial/devel/preproc.html

#+ ---------------------------------
#' ## mlr benchmarking
#' 
#+ benchmarking, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# Now we can make a benchmark experiment as described in mlr package.
# This will allow us to choose the best learner for a specific task.
# Later, we will need to also define the best task among many tasks (tasks differ by the features that are incorporated)
# we also have the option to use an automatic feature selector by fusing it to the learner (see mlr doc).
# defining the learner who will be used by taking the one with the lowest RMSE from the bmr experiment

bmr.l <- benchmark(
  learners = lrns.l,
  tasks = stations.nested.df$all_vars.stations.task2,
  resamplings = resampling.l,
  keep.pred = TRUE,
  show.info = TRUE
)

#+ ---------------------------------
#' ## Defining the best method (i.e. combination of task + learner)
#' 
#+ best method, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# mlr provides a function to access bmr performance aggregated or not. But in mlr the agreggation level is limited to each 30 stations hourly set.
# => We need to compute the global mean of the performance measures 
perfs.methods.df <- getBMRAggrPerformances(bmr.l,as.df = TRUE)

global.methods.perfs <- perfs.methods.df %>% 
  group_by(learner.id) %>%
  dplyr::summarise(global = mean(mse.test.mean))
  
# extracting the best learner from the bmr.l
# ::todo:: combination of tsaks
best.method <- getBMRLearners(bmr.l)[names(getBMRLearners(bmr.l)) == filter(global.methods.perfs, global == min(global))$learner.id]

# extracting one of the learner from the bmr expe
resp.regr.lrn = class(getBMRLearners(bmr.l))[[1]] #extracting the first learner of the bmr experiment

#+ ---------------------------------
#' ## Spatial prediction for one hour
#' 
#+ one_h_spatial_pred, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# defining the regression prediction tasks on the interpolation grid for each of the hourly datasets
tsa.nested.df <- tsa.nested.df %>%
  mutate(grid_task = purrr::map2(
    as.character(mtime),
    data_as_df,
    mlr::makeRegrTask,
    target = "tsa"
  )
  )

# defining the standard error learner by altering the previous one.
# We need it to make a map that combines prediction with uncertainty
se.regr.lrn = setPredictType(resp.regr.lrn, "se")



# defining the models for each of the hourly datasets
tsa.nested.df <- tsa.nested.df %>%
  mutate(model = purrr::map2(
    task,
    train,
    mlr::makeRegrTask,
    target = target.chr
  )
  )

# predicting one hour
spatialized.df <- tsa.records.df %>%
  filter(mtime == tsa.records.df$mtime[13] ) %>%
  spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = explanatory.df
  )

# making it a spatial object 
# https://www.rdocumentation.org/packages/sp/versions/1.2-7/topics/SpatialGridDataFrame-class
#https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates#29736844
gridded <- spatialized.df
coordinates(gridded) <- c("longitude", "latitude")
crs(gridded) <- crs(grid.sp)

gridded.3812.sp <- spTransform(gridded, CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))
gridded.3812.sp  <- as(gridded.3812.sp , "SpatialPixelsDataFrame")
gridded.3812.sp <- as(gridded.3812.sp, "SpatialGridDataFrame")

#+ ---------------------------------
#' ## mapping the predicted grid using tmap
#' 
#+ mapping, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

be.sp <- getData('GADM', country = 'BE', level = 1, download = TRUE)
be.sp$NAME_1
wallonie.sp <- be.sp[be.sp$NAME_1 == "Wallonie",]

# check the CRS to know which map units are used  
proj4string(wallonie.sp)

# set CRS to "lambert 2008" which EPSG is 3812
wallonie.3812.sp <- spTransform(wallonie.sp, CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))

library(tmap)
tm_shape(gridded.3812.sp, projection="3812") +
  tm_raster("response",  
            palette = "OrRd",
            title="Temperature",
            auto.palette.mapping=FALSE,
            breaks = seq(min(gridded.3812.sp@data$response, na.rm = TRUE),
                         max(gridded.3812.sp@data$response, na.rm = TRUE),
                         length = 11)) +
  tm_layout(legend.position = c(0.01,0.2),
            legend.text.size = 0.7) +
  tm_compass(position = c(0.2,0.2), color.light = "grey20") +
  tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",
               color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.2,0.01),
               just = NA) +
  tm_shape(wallonie.3812.sp) +
  tm_borders("grey20", lwd = 1.5) +
  tm_credits("© CRA-W", position = c(.85, 0))




# Now we need to make prediction for new data (i.e. our spatial grid).
# Ideally, prediction should be made using the best method thanks to results of bencmark experiment,
# but for now, we can predict (spatialize for all of them just to test the code)
# https://mlr-org.github.io/mlr/reference/predict.WrappedModel.html
# We need to call predict on our wrapped models. Let's extarct them :
# https://mlr-org.github.io/mlr/articles/tutorial/devel/benchmark_experiments.html
fitted_models.l  <- getBMRModels(tsa.bmr.l)

#' # selecting features useful for modelization
#' tsa.model.df <- tsa.records.df %>% select(one_of(c("longitude", "latitude", "altitude", "tsa")))
#' 
#' # converting to sf
#' tsa.model.sf <- sf::st_as_sf(x = tsa.model.df, 
#'                              coords = c("longitude", "latitude"),
#'                              crs = 4326)
#' 
#' #+ ---------------------------------
#' #' ## Spatialization
#' #' 
#' #+ spatialization, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
#' 
#' make_sf <- function(row){
#'   st_as_sf(
#'     x = row, 
#'     coords = c("longitude", "latitude"),
#'     crs = 4326)
#' }
#' 
#' #https://stackoverflow.com/questions/35558766/purrr-map-a-t-test-onto-a-split-df
#' #http://stat545.com/block024_group-nest-split-map.html
#' #https://purrr.tidyverse.org/reference/map.html
#' #https://stackoverflow.com/questions/47415072/pass-multiple-functions-to-purrrmap
#' #https://stackoverflow.com/questions/42518156/use-purrrmap-to-apply-multiple-arguments-to-a-function#42518473
#' #https://stackoverflow.com/questions/49724457/how-to-pass-second-parameter-to-function-while-using-the-map-function-of-purrr-p
#' #https://gis.stackexchange.com/questions/222978/lon-lat-to-simple-features-sfg-and-sfc-in-r
#' 
#' 
#' 
#' 
#' #One model for each hour
#' mod.by_mtime <- tsa.records.df %>%
#'   group_by(mtime) %>%
#'   by_row(spatialize(
#'     records.df  = .,
#'     task.id.chr = "t",
#'     learner.id.chr = "l",
#'     learner.cl.chr = "regr.lm",
#'     target.chr = "tsa",
#'     prediction_grid.df = vn_1_data.grid.df
#'   ))
#' 
#' 
#' 
#' 
#' mod.by_mtime.sf <-mod.by_mtime %>%
#'   do(make_sf(
#'     row =.
#'   ))
#' 
#' bmr.by_mtime <- tsa.records.df %>%
#'   group_by(mtime) %>%
#'   do(lrns.benchmark(
#'     records.df = .,
#'     task.id.chr = "t",
#'     target.chr = "tsa",
#'     prediction_grid.df = vn_1_data.grid.df
#'   ))
#' 
#' # 
#' se <- spatialize(
#'   records.df = tsa.records.df,
#'   task.id.chr = "t",
#'   learner.id.chr = "l",
#'   learner.cl.chr = "regr.lm",
#'   target.chr = "tsa",
#'   prediction_grid.df = vn_1_data.grid.df
#' )



# http://stat545.com/block023_dplyr-do.html
# https://gis.stackexchange.com/questions/224915/extracting-data-frame-from-simple-features-object-in-r
# http://mlr-org.github.io/mlr/articles/tutorial/devel/handling_of_spatial_data.html
# http://www.bisolutions.us/A-Brief-Introduction-to-Spatial-Interpolation.php



#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  


