source("./R/file_management.R")
source_files_recursively.fun("./R")
source_files_recursively.fun("./ld_devs/explorations/mlr_preparation/R")
source_files_recursively.fun("./ld_devs/explorations/solar_irradiance/R")

library(tidyverse)
library(sf)
library(mlr)

# Retrieving from API
records.stations.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "1,4,7,9,10,13,14,15,17,18,19,23,24,25,26,27,29,30,32,33,34,35,37,38,39,40,41,42,61",
    sensors.chr = "tsa,ens",
    dfrom.chr = "2018-04-01",
    dto.chr = "2018-05-25",
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


load("./data/expl.static.stations.sf.rda")
# Filtering static records to keep only the useful ones
expl.static.stations.sf <- expl.static.stations.sf %>%
  filter(gid != 36 & gid != 41)

# Selecting only the useful features
records.stations.df <- records.stations.df %>%
  dplyr::select("mtime", "sid", "ens", "longitude", "latitude", "tsa")
colnames(records.stations.df)[2] <- "gid"

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
                                           filter_abs.num = 2)

data.stations.n.df <- data.stations.n.df %>%
  mutate(ens.herb.task = purrr::map2(
    paste0(as.character(mtime),"-ens.herb"),
    data_as_df,
    mlr::makeRegrTask,
    target = "tsa"
  ))
data.stations.n.df$alt.task <- purrr::map(data.stations.n.df$alt.task, dropFeatures, features = c("slope", "aspect", "roughness", "crops", "artificial", "forest", "herbaceous", "X", "Y", "ens"))
data.stations.n.df$ens.task <- purrr::map(data.stations.n.df$ens.task, dropFeatures, features = c("slope", "aspect", "roughness", "crops", "artificial", "forest", "herbaceous", "X", "Y", "altitude"))
data.stations.n.df$alt.ens.task <- purrr::map(data.stations.n.df$alt.ens.task, dropFeatures, features = c("slope", "aspect", "roughness", "crops", "artificial", "forest", "herbaceous", "X", "Y"))
data.stations.n.df$crops.task <- purrr::map(data.stations.n.df$crops.task, dropFeatures, features = c("slope", "aspect", "roughness", "ens", "altitude", "artificial", "forest", "herbaceous", "X", "Y"))
data.stations.n.df$herb.task <- purrr::map(data.stations.n.df$herb.task, dropFeatures, features = c("slope", "aspect", "roughness", "crops", "artificial", "forest", "ens", "altitude", "X", "Y"))
data.stations.n.df$artif.task <- purrr::map(data.stations.n.df$artif.task, dropFeatures, features = c("slope", "aspect", "roughness", "crops", "altitude", "ens", "forest", "herbaceous", "X", "Y"))
data.stations.n.df$ens.crops.task <- purrr::map(data.stations.n.df$ens.crops.task, dropFeatures, features = c("slope", "aspect", "roughness", "altitude", "artificial", "forest", "herbaceous", "X", "Y"))
data.stations.n.df$ens.herb.task <- purrr::map(data.stations.n.df$ens.herb.task, dropFeatures, features = c("slope", "aspect", "roughness", "altitude", "artificial", "forest", "crops", "X", "Y"))
data.stations.n.df$ens.alt.crops.task <- purrr::map(data.stations.n.df$ens.alt.crops.task, dropFeatures, features = c("slope", "aspect", "roughness", "artificial", "forest", "herbaceous", "X", "Y"))


# defining the learner
# lrn <- list(makeFilterWrapper(makeLearner(cl = "regr.lm", id="linear regression"),
#                               fw.method = "linear.correlation", threshold = 0.25))
lrn <- list(makeLearner(cl = "regr.lm", id = "linear regression"))

lrns.l <- list(makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.alt"), fw.method = "linear.correlation", fw.mandatory.feat = "altitude", fw.abs = 1),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.ens"), fw.method = "linear.correlation", fw.mandatory.feat = "ens", fw.abs = 1),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.alt.ens"), fw.method = "linear.correlation", fw.mandatory.feat = c("ens", "altitude"), fw.abs = 2))


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

bmr.w.l <- benchmark(
  learners = lrns.l,
    tasks = data.stations.n.df$tasks,
    # list(data.stations.n.df$alt.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #      data.stations.n.df$ens.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #          data.stations.n.df$alt.ens.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #          data.stations.n.df$crops.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #          data.stations.n.df$herb.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #          data.stations.n.df$artif.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #          data.stations.n.df$ens.crops.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #          data.stations.n.df$ens.herb.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]],
    #          data.stations.n.df$ens.alt.crops.task[[which(data.stations.n.df$mtime=="2018-04-22 09:00:00")]]),
  resamplings = resampling.l,
  keep.pred = T,
  show.info = FALSE,
  models = FALSE,
  measures = list(rmse, timetrain)
)

# plotBMRBoxplots(bmr.l, measure = mse, order.tsks = getBMRTaskIds(bmr.l))
plotBMRRanksAsBarChart(bmr.w.l, pretty.names = F)
# mlr::plotLearnerPrediction(learner = lrn[[1]],
                           # task = data.stations.n.df$filtered_tasks[[which(data.stations.n.df$mtime=="2018-04-10 11:00:00")]])
ggplotly(plotBMRSummary(bmr.w.l, measure = rmse, pointsize = 1, pretty.names = F))

bmr.comp.l <- mergeBenchmarkResults(list(bmr.ens.l, bmr.alt.ens.l))

# computing performances of the benchmark
perfs.methods.df <- getBMRAggrPerformances(bmr.w.l,as.df = TRUE, rmse)
perfs.mean.df <- data.frame(rmse.mean = c(mean(perfs.methods.df$rmse.test.rmse[perfs.methods.df$learner.id=="lm.alt.filtered"]),
                                          mean(perfs.methods.df$rmse.test.rmse[perfs.methods.df$learner.id=="lm.ens.filtered"]),
                                          mean(perfs.methods.df$rmse.test.rmse[perfs.methods.df$learner.id=="lm.alt.ens.filtered"]),
                                          mean(perfs.methods.df$rmse.test.rmse)),
                            method = c("lm.alt", "lm.ens", "lm.alt.ens", "mean"))
# predicting temperature
# tsa.predict.df <- getBMRPredictions(bmr.l, as.df = TRUE)

# get coefficents from regression model equation
data.stations.n.df <- data.stations.n.df %>%
  mutate(regr.model = map(
    filtered_tasks,
    train,
    learner = lrn[[1]]
  ))
data.stations.n.df <- data.stations.n.df %>%
  mutate(get.model = map(
    regr.model,
    getLearnerModel
  ))


# spatialize prediction and error on the grid of Wallonia
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
load("./data/dssf.0103_3105.df.rda")
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

# Create the virtual stations interpolation grid points
# grid.1000.pg.sf <- build.vs.grid.fun(
#   res.num = 1000,
#   geom.chr = "polygons",
#   sf.bool = TRUE,
#   EPSG.chr = "3812",
#   country_code.chr = "BE",
#   NAME_1.chr = "Wallonie"
# )
# spatialized.tsa_error.pg.sf <- build.spatialized.tsa_error.pg(spatialized.tsa_error.sf = spatialized.tsa_error.sf,
#                                                             grid.pg.sf = grid.1000.pg.sf,
#                                                             sf.bool = TRUE)

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

ggmap <- static.ggmap(gridded.data.df =spatialized.tsa_error.df,
             boundaries.sf = boundaries.sf,
             layer.error.bool = T,
             legend.error.bool = F,
             pretty_breaks.bool = T,
             title.chr = "Interpolated temperature with multiple linear regression - 2018-05-02 14:00:00",
             target.chr = "tsa")
ggmap

### Investigation clc

# load clc data
library(raster)
cover.sf <- build_cover.sf.fun(country_code.chr = "BE",
                               NAME_1.chr = "Wallonie", 
                               EPSG.chr = "3812", 
                               path.corine.shapefile.chr = "./external-data/Corine_Land_Cover/CLC12_BE.shp",
                               EPSG.corine.chr = "3812"
)
cover_agri_herb.sf <- cover.sf %>%
  dplyr::filter(CLASS == "Agricultural areas" | CLASS == "Herbaceous vegetation")

intersect.clc_agri_herb.tsa_error.sf <- st_intersection(st_buffer(cover_agri_herb.sf, dist = 0), spatialized.tsa_error.pg.sf)

mean(spatialized.tsa_error.pg.sf$se, na.rm = TRUE)
mean(intersect.clc_agri_herb.tsa_error.sf$se, na.rm = TRUE)

comparison.error.14h.55d.df <- data.frame(all = c(0.2219171, 0.223256, 0.229797, 0.2698662),
                                  agri_herb = c(0.2172349, 0.2185456, 0.2249486, 0.2632026))
comparison.error.02h.55d.df <- data.frame(all = c(0.1409509, 0.5373542, 0.3102381, 2.237924, 2.121635),
                                      agri_herb = c(0.137977, 0.5019015, 0.3046453, 1.496699, 1.418927))
comparison.error.21h.55d.df <- data.frame(all = c(0.3865789, 0.3772858, 0.4651825, 0.3557736, 1.813699),
                                      agri_herb = c(0.3784226, 0.3693255, 0.4553678, 0.3469886, 1.23285))
comparison.error.mean.55d.df <- data.frame(hour = c("02:00:00", "14:00:00", "21:00:00"),
                                       all = c(mean(comparison.error.02h.55d.df$all), mean(comparison.error.14h.55d.df$all), mean(comparison.error.21h.55d.df$all)),
                                       agri_herb = c(mean(comparison.error.02h.55d.df$agri_herb), mean(comparison.error.14h.55d.df$agri_herb), mean(comparison.error.21h.55d.df$agri_herb)))

map_comparison <- create_map_tsa.comparison_clc(spatial_data.sf = spatialized.tsa_error.sf,
                                                clc.sf = cover_agri_herb.sf,
                                                country_code.chr = "BE", NAME_1.chr = "Wallonie")
map_comparison


### test comparison ens stations and ens eumetsat

comparison.ens.sf <- st_intersection(st_buffer(st_as_sf(expl.static.grid.df,
                                                        coords = c("X", "Y"),
                                                        crs = lambert2008.crs), dist = 500),
                                     records.stations.sf) %>%
  dplyr::filter(mtime == "2018-05-02 14:00:00")

dssf.0103_3105.l.df <- dssf.0103_3105.df %>%
  st_as_sf(., coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(., crs = lambert2008.crs)
dssf.0103_3105.l.df <- dplyr::bind_cols(dssf.0103_3105.l.df, data.frame(sf::st_coordinates(dssf.0103_3105.l.df)))

dssf.n.l.df <- dssf.0103_3105.l.df %>%
  group_by(mhour) %>%
  nest()

dssf.red.n.df <- dssf.n.l.df %>%
  filter(mhour >= "2018-04-01 00:00:00" & mhour <= "2018-05-25 00:00:00")

data_ens.df <- left_join(dssf.red.n.df, data.stations.n.df, by = c("mhour" = "mtime")) %>%
  dplyr::select(mhour, data, data_as_df)

data_ens.df <- data_ens.df %>%
  mutate(comparison_ens = purrr::map2(
    data,
    data_as_df,
    left_join,
    by = c("X", "Y")
  ))




