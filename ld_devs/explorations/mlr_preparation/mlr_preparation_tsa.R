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
                                           target.chr = "tsa")
filtered.tasks <-  purrr::map(
  data.stations.n.df$tasks,
  mlr::filterFeatures,
  method = "linear.correlation",
  perc = 0.25
)

# dropFeatures(data.stations.n.df$tasks[[1]], "hra")
# data.stations.n.df <- map(
#   .x = data.stations.n.df$tasks,
#   .f = mlr::dropFeatures,
#   task = tasks,
#   features = "hra"
# )

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

perfs.methods.df <- getBMRAggrPerformances(bmr.l,as.df = TRUE)
tsa.predict.df <- getBMRPredictions(bmr.l, as.df = TRUE)

load("./data/expl.static.grid.df.rda")
spatialized.tsa.sf <- spatialize(learner.cl.chr = "regr.lm",
                                 learner.id.chr = "linear regression",
                                 task = filtered.tasks[[20]],
                                 prediction_grid.df = expl.static.grid.df,
                                 predict.type = "response"
                                 )

spatialized.tsa.sp <- as(spatialized.tsa.sf, "Spatial") %>%
  as(., "SpatialPixelsDataFrame") %>%
  as(., "SpatialGridDataFrame")

spatialized.error.sf <- spatialize(learner.cl.chr = "regr.lm",
                                 learner.id.chr = "linear regression",
                                 task = filtered.tasks[[20]],
                                 prediction_grid.df = expl.static.grid.df,
                                 predict.type = "se"
                                 ) %>%
  dplyr::select(gid, geometry, response, se)


# Set the visualization values:
# resp.min - lower limit value;
# resp.max - upper limit value;
# se.min - lower error value (0.4);
# se.max - upper error value (1.0);
# var - variance in the sampled data;

resp.min <- min(spatialized.tsa.sf$response, na.rm=TRUE)
resp.max <- max(spatialized.tsa.sf$response, na.rm=TRUE)
se.min <- min(spatialized.error.sf$se, na.rm=TRUE)
se.max <- max(spatialized.error.sf$se, na.rm=TRUE)

# Derive the normalized error:

spatialized.error.sf$er <- sqrt(spatialized.error.sf$se)/sqrt(var(log(spatialized.tsa.sf$response)))

# Strech the values (z) to the inspection range:
# Mask the values out of the 0-1 range:

spatialized.error.sf$norm.response <- (spatialized.error.sf$response-resp.min)/(resp.max-resp.min)
spatialized.error.sf$norm.response.centred <- ifelse(spatialized.error.sf$norm.response<=0, 0, ifelse(spatialized.error.sf$norm.response>1, 1, spatialized.error.sf$norm.response))

# Derive the Hue image:
# The hues should lie between between 0 and 360, and the saturations
# and values should lie between 0 and 1.

spatialized.error.sf$tmpf1 <- -90-spatialized.error.sf$norm.response.centred*300
spatialized.error.sf$tmpf2 <- ifelse(spatialized.error.sf$tmpf1<=-360, spatialized.error.sf$tmpf1+360, spatialized.error.sf$tmpf1)
spatialized.error.sf$H <- ifelse(spatialized.error.sf$tmpf2>=0, spatialized.error.sf$tmpf2, (spatialized.error.sf$tmpf2+360))
spatialized.error.sf$H <- spatialized.error.sf$H/360

# Strech the error values (e) to the inspection range:
# Mask the values out of the 0-1 range:

spatialized.error.sf$norm.se <- (spatialized.error.sf$se-se.min)/(se.max-se.min)
spatialized.error.sf$norm.se.centred <- ifelse(spatialized.error.sf$norm.se<=0, 0, ifelse(spatialized.error.sf$norm.se>1, 1, spatialized.error.sf$norm.se))

# Derive the saturation and intensity images:

spatialized.error.sf$S <- 1-spatialized.error.sf$norm.se.centred
spatialized.error.sf$V <- 0.5*(1+spatialized.error.sf$norm.se.centred)

# Convert the HSV values to RGB and put them as R, G, B bands:

library(colorspace)
RGBimg <- as(HSV(spatialized.error.sf$H, spatialized.error.sf$S, spatialized.error.sf$V), "RGB")
summary(RGBimg@coords)
# library(rgdal)
# RGBimg <- SGDF2PCT(spatialized.error.sp[c("red", "green", "blue")], ncolors=256, adjust.bands=FALSE)
# spatialized.error.sp$idx <- RGBimg$idx
# image(spatialized.error.sp, "idx", col=RGBimg$ct)

spatialized.error.sf$red <- RGBimg@coords[,1]*255
spatialized.error.sf$green <- RGBimg@coords[,2]*255
spatialized.error.sf$blue <- RGBimg@coords[,3]*255
summary(spatialized.error.sf[c("red", "green", "blue")])
spatialized.error.sf$RGB <- rgb(spatialized.error.sf$red/255, spatialized.error.sf$green/255, spatialized.error.sf$blue/255)
colors <- sort(unique(spatialized.error.sf$RGB), decreasing = TRUE)


spatialized.error.sp <- as(spatialized.error.sf, "Spatial") %>%
  as(., "SpatialPixelsDataFrame") %>%
  as(., "SpatialGridDataFrame")

load("./data/wallonie.3812.sp.rda")

map <- create_map_tsa(spatial_data.sp = spatialized.error.sp,
                      grid.sp = wallonie.3812.sp,
                      method.chr = "linear regression",
                      date.chr = "2018-05-01 19:00:00",
                      type.chr = "static",
                      country_code.chr = "BE",
                      NAME_1.chr = "Wallonie"
                      )
map




















