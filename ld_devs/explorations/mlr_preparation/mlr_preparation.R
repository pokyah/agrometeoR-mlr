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

# install.packages('FSelector')
# install.packages('elmNN')
# install.packages('kknn')
library(FSelector)
library(tidyverse)
library(sf)
library(mlr)


#+ ---------------------------------
#' ## Data acquisition
#'
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#' ### Dependent variables

# static variables for stations

# load("./data/expl.static.stations.sf.rda")
load("./data/expl.static.stations.sf.rda")

####  ####
# Retrieving from API
records.stations.df <- prepare_agromet_API_data.fun(
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

# Filtering dynamic records to keep only the useful ones
records.stations.df <- records.stations.df %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(!is.na(to)) %>%
  filter(state == "Ok") %>%
  filter(!is.na(tsa)) %>%
  filter(!is.na(ens))

# Filtering static records to keep only the useful ones
expl.static.stations.sf <- expl.static.stations.sf %>%
  filter(gid != 36 & gid != 41)

# Selecting only the useful features
records.stations.df <- records.stations.df %>%
  dplyr::select("mtime", "sid", "tsa" ,"ens", "longitude", "latitude")
colnames(records.stations.df)[2] <- "gid"

# Preparing for spatial join of dynamic and static expl vars
records.stations.sf <- st_as_sf(records.stations.df, coords = c("longitude", "latitude"))
st_crs(records.stations.sf) <- "+proj=longlat +datum=WGS84 +no_defs"
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
records.stations.sf <- st_transform(records.stations.sf, crs = lambert2008.crs)
data.sf <- st_join(expl.static.stations.sf, records.stations.sf)

# Make the sf objects, df again
data.stations.df <- data.frame(data.sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::select(-gid.y) %>%
  dplyr::select(-gid.x)

# Building a nested data frame, where for each hourly observation we have a 27 stations dataset of 1h record.
data.stations.n.df <- data.stations.df %>%
  group_by(mtime) %>%
  tidyr::nest() %>%
  filter(mtime <= "2018-06-11 23:00:00" & mtime >= "2018-06-11 00:00:00")

# converting each tibble of the nested records to a strict dataframe (required by mlr)
# ::todo:: need to use transmute_at
data.stations.n.df <- data.stations.n.df %>%
  mutate(data_as_df = purrr::map(
    .x = data,
    .f = data.frame
  ))

# removing the tibbles columns and only keeping the pure dataframes (required by mlr)
data.stations.n.df <- data.stations.n.df %>%
  dplyr::select(mtime, data_as_df)

#+ ---------------------------------
#' ## mlr benchmark experiment preparation
#'
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# defining the target var
target.chr = "tsa"

# defining the learners who will be compared
lrns.l <- list(
  # makeFilterWrapper(
  # learner = makeLearner(cl = "regr.lm", id="linear regression"),  fw.method = "information.gain", fw.abs = 2),
  makeLearner(cl = "regr.lm", id="linear regression"),
  makeLearner(cl = "regr.elmNN", id="single layer neural net"),
  makeLearner(cl ="regr.kknn", id="nearest neighbours")
  # makeLearner(cl = "regr.km", id="kriging")
)

# defining the validation (resampling) strategy
resampling.l = mlr::makeResampleDesc(
  method = "LOO"#,
  #predict = "test"
)

# defining the regression tasks on the stations observations for each of the hourly datasets
# https://stackoverflow.com/questions/46868706/failed-to-use-map2-with-mutate-with-purrr-and-dplyr
# https://stackoverflow.com/questions/42518156/use-purrrmap-to-apply-multiple-arguments-to-a-function?rq=1
data.stations.n.df <- data.stations.n.df %>%
  mutate(tasks = purrr::map2(
    as.character(mtime),
    data_as_df,
    mlr::makeRegrTask,
    target = target.chr
  )
  )

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
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = FALSE,
  show.info = TRUE,
  models = FALSE,
  measures = list(mse, timetrain)
)
####  ####

bmr.test <- make.benchmark_learners(dfrom.chr = "2018-04-01", dto.chr = "2018-05-30")
perfs.methods.df <- getBMRAggrPerformances(bmr.test,as.df = TRUE)
tsa.predict.df <- getBMRPredictions(bmr.test, as.df = TRUE)

plotBMRBoxplots(bmr.test, measure = mse, style = "violin")
plotBMRSummary(bmr.test)
plotBMRRanksAsBarChart(bmr.test, pos = "dodge", order.lrn = getBMRLearnerIds(bmr.test))

# qplot(mmce, colour = learner.id, facets = . ~ task.id,
#       data = perfs.methods.30days.df[perfs.methods.30days.df$task.id %in% c("2018-05-11 12:00:00", "2018-05-11 13:00:00"),], geom = "density") +
#   theme(strip.text.x = element_text(size = 4))

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
resp.regr.lrn = class(getBMRLearners(bmr.test))[[1]] #extracting the first learner of the bmr experiment

#+ ---------------------------------
#' ## Spatial prediction for one hour
#'
#+ one_h_spatial_pred, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# defining the regression prediction tasks on the interpolation grid for each of the hourly datasets
data.stations.n.df <- data.stations.n.df %>%
  mutate(grid_task = purrr::map2(
    as.character(mtime),
    data_as_df,
    mlr::makeRegrTask,
    target = "tsa"
  )
  )

# defining the standard error learner by altering the previous one.
# We need it to make a map that combines prediction with uncertainty
se.regr.lrn = setPredictType(lrns.l, "se")


# defining the models for each of the hourly datasets
data.stations.n.df <- data.stations.n.df %>%
  mutate(model = purrr::map2(
    task,
    train,
    mlr::makeRegrTask,
    target = target.chr
  )
  )

# predicting

spatialized.df <- data.stations.n.df %>%
  spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = expl.static.stations.df
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


spatialized.7days.df <- expl.stations.nested.7days.df %>%
  spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = explanatory.df
  )
>>>>>>> b8e05524a716fae38c11b1e04ede61b7154e2fc7

spatialized.30days.df <- expl.stations.nested.30days.df %>%
  filter(mtime == expl.stations.nested.30days.df$mtime[13] ) %>%
  spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = explanatory.df
  )

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


