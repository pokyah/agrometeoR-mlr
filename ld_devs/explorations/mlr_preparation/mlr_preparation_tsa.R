source("./R/file_management.R")
source_files_recursively.fun("./R")
source_files_recursively.fun("./ld_devs/explorations/mlr_preparation/R")

library(tidyverse)
library(sf)
library(mlr)

# Retrieving from API
records.stations.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "all",
    sensors.chr = "tsa,ens,hra, hct",
    dfrom.chr = "2018-05-01",
    dto.chr = "2018-05-06",
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
  filter(!is.na(ens)) %>%
  filter (!is.na(hra)) %>%
  filter(!is.na(hct))


load("./data/expl.static.stations.sf.rda")
# Filtering static records to keep only the useful ones
expl.static.stations.sf <- expl.static.stations.sf %>%
  filter(gid != 36 & gid != 41)

# Selecting only the useful features
records.stations.df <- records.stations.df %>%
  dplyr::select("mtime", "sid", "tsa" ,"ens", "hra", "hct", "longitude", "latitude")
colnames(records.stations.df)[2] <- "gid"

# Preparing for spatial join of dynamic and static expl vars
records.stations.sf <- st_as_sf(records.stations.df, coords = c("longitude", "latitude"))
st_crs(records.stations.sf) <- "+proj=longlat +datum=WGS84 +no_defs"
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
records.stations.sf <- st_transform(records.stations.sf, crs = lambert2008.crs)

# Make tasks
data.stations.n.df <- make.benchmark.tasks(static.vars = expl.static.stations.sf,
                                           dynamic.vars = records.stations.sf,
                                           target.chr = "tsa",
                                           feat_to_drop.chr = c("hra", "hct"),
                                           filter_method.chr = "linear.correlation",
                                           filter_abs.num = 3)

# defining the learner
# lrn <- list(makeFilterWrapper(makeLearner(cl = "regr.lm", id="linear regression"),
#                               fw.method = "linear.correlation", threshold = 0.25))
lrn <- list(makeLearner(cl = "regr.lm", id = "linear regression"))

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
  learners = lrn,
  tasks = filtered.tasks,
  resamplings = resampling.l,
  keep.pred = TRUE,
  show.info = FALSE,
  models = FALSE,
  measures = list(mse, timetrain)
)

# # computing performances of the benchmark
# perfs.methods.df <- getBMRAggrPerformances(bmr.l,as.df = TRUE)
# 
# # predicting temperature
# tsa.predict.df <- getBMRPredictions(bmr.l, as.df = TRUE)

# spatialize prediction and error on the grid of Wallonia
load("./data/expl.static.grid.df.rda")
spatialized.tsa_error.sf <- spatialize(learner.cl.chr = "regr.lm",
                                 learner.id.chr = "linear regression",
                                 task = data.stations.n.df$filtered_tasks[[which(data.stations.n.df$mtime == '2018-05-02 14:00:00')]],
                                 prediction_grid.df = expl.static.grid.df,
                                 predict.type = "se"
                                 ) %>%
  dplyr::select(gid, geometry, response, se)

# load grid !
spatialized.tsa_error.pg.sf <- build.spatialized.tsa_error.pg(spatialized.tsa_error.sf = spatialized.tsa_error.sf,
                                                            grid.pg.sf = grid.1000.pg.sf,
                                                            sf.bool = FALSE)

# visualize the map
# function needs a sf made of points if you do not want to visualize error
# but needs a sf made of polygons if you want to
map <- create_map_tsa(spatial_data.sf = spatialized.tsa_error.pg.sf,
                      method.chr = "linear regression",
                      date.chr = "2018-05-01 19:00:00",
                      type.chr = "interactive",
                      country_code.chr = "BE",
                      NAME_1.chr = "Wallonie",
                      error.bool = TRUE
                      )
map
















