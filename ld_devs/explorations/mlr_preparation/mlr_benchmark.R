source("./R/file_management.R")
# source_files_recursively.fun("./R")
source_files_recursively.fun("./ld_devs/explorations/mlr_preparation/R")
source_files_recursively.fun("./ld_devs/explorations/solar_irradiance/R")

library(geoTools)
library(agrometAPI)
library(tidyverse)
library(sf)
library(mlr)
library(plotly)
library(jsonlite)

### get records from API and solar irradiance from dssf
# get dssf records
dssf.l <- jsonlite::fromJSON("")
dssf.df <- dssf.l$features

# make each feature properties a list element
date_ens.l <- dssf.df$properties
date_ens.l <- purrr::imap(date_ens.l, function(x, sid) cbind(x, sid))
coords.l <- dssf.df$geometry$coordinates
coords.l <- lapply(coords.l, function(x) as.data.frame(t(x)))
coords.l <- purrr::imap(coords.l, function(x, sid) cbind(x, sid))
# join each feature coordinates + properties
# https://stackoverflow.com/questions/44703738/merge-two-lists-of-dataframes
dssf.df <- purrr::map2_df(date_ens.l, coords.l, dplyr::left_join, by="sid")
colnames(dssf.df) <- c("mhour", "ens", "sid", "lat", "lon")
# Create a dataframe for the stations meta-information
stations_meta.df <- dssf.l$references$stations
# Group in a list
dssf_and_stations_meta.l <- list(stations_meta.df = stations_meta.df, records.df = dssf.df)
dssf.df <- prepare_agromet_API_data.fun(dssf_and_stations_meta.l, "get_rawdata_dssf")

# get tsa and ens records from API
records.l <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/cleandataSensorstsaForallFm2015-11-11To2018-06-30.json")
records.df <- records.l$results
stations_meta.df <- records.l$references$stations
records_and_stations_meta.l <- list(stations_meta.df = stations_meta.df, records.df = records.df)
records.stations.df <- prepare_agromet_API_data.fun(records_and_stations_meta.l, "cleandata")
# Filtering dynamic records to keep only the useful ones
records.stations.df <- records.stations.df %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(!is.na(to)) %>%
  filter(state == "Ok") %>%
  filter(!is.na(tsa)) %>%
  filter(!is.na(ens))

# Selecting only the useful features
records.stations.df <- records.stations.df %>%
  dplyr::select("mtime", "sid", "ens", "longitude", "latitude", "tsa")
colnames(records.stations.df)[2] <- "gid"

# get static variables
load("./data/expl.static.stations.sf.rda")
# Filtering static records to keep only the useful ones
expl.static.stations.sf <- expl.static.stations.sf %>%
  filter(gid != 36 & gid != 41)

# Preparing for spatial join of dynamic and static expl vars
records.stations.sf <- st_as_sf(records.stations.df, coords = c("longitude", "latitude"))
st_crs(records.stations.sf) <- "+proj=longlat +datum=WGS84 +no_defs"
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
records.stations.sf <- st_transform(records.stations.sf, crs = lambert2008.crs)
records.stations.sf <- dplyr::bind_cols(records.stations.sf, data.frame(sf::st_coordinates(records.stations.sf)))

# Make tasks
data.stations.n.df <- make.benchmark.tasks(static.vars = expl.static.stations.sf,
                                           dynamic.vars = records.stations.sf,
                                           target.chr = "tsa",
                                           feat_to_drop.chr = NULL,
                                           filter_method.chr = "linear.correlation",
                                           filter_abs.num = 2,
                                           mandatory.feat.chr = NULL)

# defining the learner
# lrn <- list(makeLearner(cl = "regr.lm", id = "linear regression"))
lrns.l <- list(makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.alt"), fw.method = "linear.correlation", fw.mandatory.feat = "altitude", fw.abs = 1),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.ens"), fw.method = "linear.correlation", fw.mandatory.feat = "ens", fw.abs = 1),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.alt.ens"), fw.method = "linear.correlation", fw.mandatory.feat = c("ens", "altitude"), fw.abs = 2),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.ens.herb"), fw.method = "linear.correlation", fw.mandatory.feat = c("ens", "herbaceous"), fw.abs = 2),
               makeFilterWrapper(learner = makelearner(cl = "regr.lm", id = "lm.filt.abs2.ens"), fw.method = "linear.correlation", fw.mandatory.feat = "ens", fw.abs = 2),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.abs3.ens"), fw.method = 'linear.correlation', fw.mandatory.feat = "ens", fw.abs = 3),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.abs4.ens"), fw.method = 'linear.correlation', fw.mandatory.feat = "ens", fw.abs = 4),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.abs5.ens"), fw.method = 'linear.correlation', fw.mandatory.feat = "ens", fw.abs = 5),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.abs2"), fw.method = "linear.correlation", fw.abs = 2),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.abs3"), fw.method = "linear.correlation", fw.abs = 3),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.abs4"), fw.method = "linear.correlation", fw.abs = 4),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.abs5"), fw.method = "linear.correlation", fw.abs = 5),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.thr05"), fw.method = "linear.correlation", fw.threshold = 0.5),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.filt.thr03"), fw.method = "linear.correlation", fw.threshold = 0.3))


# defining the validation (resampling) strategy
resampling.l = mlr::makeResampleDesc(
  method = "LOO"#,
  #predict = "test"
)

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
  keep.pred = T,
  show.info = FALSE,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)

ggplotly(plotBMRSummary(bmr.l, measure = rmse, pointsize = 1, pretty.names = F))

# computing performances of the benchmark
perfs.methods.df <- getBMRAggrPerformances(bmr.l,as.df = TRUE)

# predicting temperature
# tsa.predict.df <- getBMRPredictions(bmr.l, as.df = TRUE)

# get coefficents from regression model equation
data.stations.n.df <- data.stations.n.df %>%
  mutate(regr.model = purrr::map(
    filtered_tasks,
    train,
    learner = lrn[[1]]
  ))
data.stations.n.df <- data.stations.n.df %>%
  mutate(get.model = purrr::map(
    regr.model,
    getLearnerModel
  ))

# create a data frame with model equation for each hour
models.df <- as.data.frame(data.stations.n.df$mtime)
colnames(models.df)[colnames(models.df) == 'data.stations.n.df$mtime'] <- 'Datetime'
for(i in 1:nrow(models.df)){
  models.df$Equation[i] <- paste0("T = ", round(data.stations.n.df$get.model[[i]][['coefficients']][['(Intercept)']], digits = 4),
                                  " + ", round(data.stations.n.df$get.model[[i]][['coefficients']][[2]], digits = 4),
                                  ".", names(data.stations.n.df$get.model[[i]][['coefficients']])[[2]],
                                  " + ", round(data.stations.n.df$get.model[[i]][['coefficients']][[3]], digits = 4),
                                  ".", names(data.stations.n.df$get.model[[i]][['coefficients']])[[3]]
  )
  models.df$Residual_SE[i] <- round(as.numeric(summary(data.stations.n.df$get.model[[i]])[['sigma']]), digits = 2)
}
models.df$Residual_SE <- as.numeric(models.df$Residual_SE)
ggplot(models.df, aes(x = Datetime, y = Residual_SE)) +
  # geom_point() +
  geom_line() +
  scale_y_continuous(breaks = round(seq(0,3, by = 0.2),1), limits = c(0,3)) +
  scale_x_datetime(date_breaks = "5 days")


### spatialize prediction and error on the grid of Wallonia
load("./data/expl.static.grid.df.rda") # be careful it is a sf but it is more efficient (::todo:: modify generate independent variables)
# expl.static.grid.df <- data.frame(dplyr::bind_cols(expl.static.grid.df, data.frame(sf::st_coordinates(expl.static.grid.df))))
grid.1000.pt.sp <- build.vs.grid.fun(
  res.num = 1000,
  geom.chr = "centers",
  sf.bool = F,
  EPSG.chr = "3812",
  country_code.chr = "BE",
  NAME_1.chr = "Wallonie"
)

# get ens on virtual grid
dssf.n.df <- dssf.0103_3105.df %>%
  group_by(mhour) %>%
  nest()
dssf.pred.df <- build.dssf.hour(dssf.n.df, "2018-05-02 14:00:00", grid.1000.pt.sp)
# add ens variable to prediction grid
expl.static.grid.df$ens <- dssf.pred.df$ens.pred
spatialized.tsa_error.sf <- spatialize(learner.cl.chr = "regr.lm",
                                       learner.id.chr = "linear regression",
                                       task = data.stations.n.df$filtered_tasks[[which(data.stations.n.df$mtime == '2018-05-02 14:00:00')]],
                                       prediction_grid.df = expl.static.grid.df,
                                       predict.type = "se"
) %>%
  dplyr::select(gid, geometry, response, se)

# visualize the map
# get Wallonia boundaries
boundaries.sp <- raster::getData('GADM', country="BE", level=1, path = "./external-data/Boundaries") %>%
  subset(NAME_1 == "Wallonie")
boundaries.sp <- spTransform(boundaries.sp, CRSobj = lambert2008.crs)
boundaries.sf <- st_as_sf(boundaries.sp)
st_crs(boundaries.sf) <- lambert2008.crs

# prepare data to be visualized (transform from points to polygons and then storing them in a df)
spatialized.tsa_error.sp <- as(spatialized.tsa_error.sf, "Spatial") %>%
  as(., "SpatialPixelsDataFrame") %>%
  as(., "SpatialGridDataFrame")
spatialized.tsa_error.df <- as.data.frame(spatialized.tsa_error.sp)

ggmap <- build.static.ggmap(gridded.data.df =spatialized.tsa_error.df,
                            boundaries.sf = boundaries.sf,
                            layer.error.bool = T,
                            legend.error.bool = F,
                            pretty_breaks.bool = T,
                            title.chr = "Interpolated temperature with multiple linear regression - 2018-05-02 14:00:00",
                            target.chr = "response",
                            legend.chr = "test",
                            nb_classes.num = 10,
                            reverse_pal.bool = TRUE)
ggmap












