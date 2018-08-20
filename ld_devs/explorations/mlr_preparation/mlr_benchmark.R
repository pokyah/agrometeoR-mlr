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
library(RColorBrewer)

# getting and preparing tsa and ens records from AGROMET API
records.l <- jsonlite::fromJSON(
  "~/Documents/code/agrometeoR-mlr/data/cleandataSensorstsa-ensForallFm2015-11-11To2018-06-30.json") # available on FTP
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

# Make tasks and filtered tasks
data.stations.n.df <- make.benchmark.tasks(static.vars = expl.static.stations.sf,
                                           dynamic.vars = records.stations.sf,
                                           target.chr = "tsa",
                                           feat_to_drop.chr = NULL,
                                           filter_method.chr = "linear.correlation",
                                           filter_abs.num = 2, # number of best variables to keep according to filter method score
                                           mandatory.feat.chr = NULL) # variable forced as explanatory variable for filtered tasks

# defining the learners
# lrn <- list(makeLearner(cl = "regr.lm", id = "linear regression"))
lrns.l <- list(makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.Long.Lat", predict.type = "se"),
                                 fw.method = "linear.correlation", fw.mandatory.feat = c("X", "Y"), fw.abs = 2),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.Long.Lat.Elev", predict.type = "se"), 
                                 fw.method = "linear.correlation", fw.mandatory.feat = c("X", "Y", "altitude"), fw.abs = 3),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.SolarIrr+1bestVar", predict.type = "se"), 
                                 fw.method = "linear.correlation", fw.mandatory.feat = "ens", fw.abs = 2),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.SolarIrr+2bestsVar", predict.type = "se"), 
                                 fw.method = 'linear.correlation', fw.mandatory.feat = "ens", fw.abs = 3),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.SolarIrr+3bestsVar", predict.type = "se"), 
                                 fw.method = 'linear.correlation', fw.mandatory.feat = "ens", fw.abs = 4),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.2bestsVar", predict.type = "se"), 
                                 fw.method = "linear.correlation", fw.abs = 2),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.3bestsVar", predict.type = "se"), 
                                 fw.method = "linear.correlation", fw.abs = 3),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.4bestsVar", predict.type = "se"), 
                                 fw.method = "linear.correlation", fw.abs = 4),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.Vars.r>0,5", predict.type = "se"), 
                                 fw.method = "linear.correlation", fw.threshold = 0.5),
               makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.Vars.r>0,3", predict.type = "se"), 
                                 fw.method = "linear.correlation", fw.threshold = 0.3))


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

# bmr are heavy : 1,3 GB if keep pred = TRUE, try to save objects and remove from environment to avoid crash

bmr.Long_Lat.p.l <- benchmark(
  learners = lrns.l[1],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T, # to merge benchmarks results you need to keep pred
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.Long_Lat.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.Long_Lat.p.l.rda")
rm("bmr.Long_Lat.p.l")

bmr.Long_Lat_Elev.p.l <- benchmark(
  learners = lrns.l[2],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T,
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.Long_Lat_Elev.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.Long_Lat_Elev.p.l.rda")
rm("bmr.Long_Lat_Elev.p.l")

bmr.SolarIrr_1bestVar.p.l <- benchmark(
  learners = lrns.l[3],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T,
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.SolarIrr_1bestVar.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.SolarIrr_1bestVar.p.l.rda")
rm("bmr.SolarIrr_1bestVar.p.l")

bmr.SolarIrr_2bestsVar.p.l <- benchmark(
  learners = lrns.l[4],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T,
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.SolarIrr_2bestsVar.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.SolarIrr_2bestsVar.p.l.rda")
rm("bmr.SolarIrr_2bestsVar.p.l")

bmr.SolarIrr_3bestsVar.p.l <- benchmark(
  learners = lrns.l[5],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T,
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.SolarIrr_3bestsVar.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.SolarIrr_3bestsVar.p.l.rda")
rm("bmr.SolarIrr_3bestsVar.p.l")

bmr.2bestsVar.p.l <- benchmark(
  learners = lrns.l[6],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T,
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.2bestsVar.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.2bestsVar.p.l.rda")
rm("bmr.2bestsVar.p.l")

bmr.3bestsVar.p.l <- benchmark(
  learners = lrns.l[7],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T,
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.3bestsVar.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.3bestsVar.p.l.rda")
rm("bmr.3bestsVar.p.l")

bmr.4bestsVar.p.l <- benchmark(
  learners = lrns.l[8],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = T,
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.4bestsVar.p.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.4bestsVar.p.l.rda")
rm("bmr.4bestsVar.p.l")

bmr.Vars.r05.l <- benchmark(
  learners = lrns.l[9],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = F, # keed pred returns error...
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.Vars.r05.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.Vars.r05.l.rda")
rm("bmr.Vars.r05.l")

bmr.Vars.r03.l <- benchmark(
  learners = lrns.l[10],
  tasks = data.stations.n.df$tasks,
  resamplings = resampling.l,
  keep.pred = F, # keep pred returns error
  show.info = T,
  models = FALSE,
  measures = list(rmse, mae, timetrain)
)
save("bmr.Vars.r03.l", file = "~/Documents/code/agrometeoR-mlr/external-data/bmr.Vars.r03.l.rda")
rm("bmr.Vars.r03.l")

# merge benchmark results, hard to merge more than 5 benchmark results !
bmrs.l <- mergeBenchmarkResults(list(bmr.Long_Lat_Elev.p.l, 
                                     bmr.SolarIrr_2bestsVar.p.l, 
                                     bmr.2bestsVar.p.l,
                                     bmr.3bestsVar.p.l,
                                     bmr.SolarIrr_1bestVar.p.l))

# plotBMRSummary(bmrs.l, measure = rmse, pointsize = 0.01, pretty.names = F) # 23089 tasks -> unreadable
# barchart displaying the number of times where each methods has rank 1 to -nb learners- to compare performances
plotBMRRanksAsBarChart(bmrs.l, pretty.names = F, measure = rmse, pos = "dodge") +
  ggtitle("Comparison of methods by their ranks") +
  scale_y_continuous(breaks = seq(0,12000,by = 2000), limits = c(0, 12000)) +
  scale_fill_discrete(labels = c("lm.2bestsVar", "lm.3bestsVar", "lm.Long.Lat.Elev", "lm.SolarIrr+1bestVar", "lm.SolarIrr+2bestsVar"), name = "Method") +
  labs(x = "Rank", y = "Number of times")

# computing performances of the benchmark
# bmrs.l <- c(bmr.Long_Lat.l, bmr.Long_Lat_Elev.l, bmr.SolarIrr_1bestVar.l, bmr.SolarIrr_2bestsVar.l,
#             bmr.SolarIrr_3bestsVar.l, bmr.2bestsVar.l, bmr.3bestsVar.l, bmr.4bestsVar.l, bmr.Vars.r05.l, bmr.Vars.r03.l)
# perfs.methods.l <- lapply(bmrs.l, getBMRAggrPerformances, as.df = TRUE)
perfs.methods.Long_Lat.df <- getBMRAggrPerformances(bmr.Long_Lat.l,as.df = TRUE)
perfs.methods.Long_Lat_Elev.df <- getBMRAggrPerformances(bmr.Long_Lat_Elev.l,as.df = TRUE)
perfs.methods.SolarIrr_1bestVar.df <- getBMRAggrPerformances(bmr.SolarIrr_1bestVar.l,as.df = TRUE)
perfs.methods.SolarIrr_2bestsVar.df <- getBMRAggrPerformances(bmr.SolarIrr_2bestsVar.l,as.df = TRUE)
perfs.methods.SolarIrr_3bestsVar.df <- getBMRAggrPerformances(bmr.SolarIrr_3bestsVar.l,as.df = TRUE)
perfs.methods.2bestsVar.df <- getBMRAggrPerformances(bmr.2bestsVar.l,as.df = TRUE)
perfs.methods.3bestsVar.df <- getBMRAggrPerformances(bmr.3bestsVar.l,as.df = TRUE)
perfs.methods.4bestsVar.df <- getBMRAggrPerformances(bmr.4bestsVar.l,as.df = TRUE)
perfs.methods.Vars.r05.df <- getBMRAggrPerformances(bmr.Vars.r05.l,as.df = TRUE)
perfs.methods.Vars.r03.df <- getBMRAggrPerformances(bmr.Vars.r03.l,as.df = TRUE)

# aggregate performances of different methods
perfs.methods.df <- rbind(perfs.methods.Long_Lat.df, perfs.methods.Long_Lat_Elev.df,
                          perfs.methods.SolarIrr_1bestVar.df, perfs.methods.SolarIrr_2bestsVar.df,
                          perfs.methods.SolarIrr_3bestsVar.df, perfs.methods.2bestsVar.df,
                          perfs.methods.3bestsVar.df, perfs.methods.4bestsVar.df,
                          perfs.methods.Vars.r05.df, perfs.methods.Vars.r03.df)

# summarise performances for each method
perfs.methods.aggr.rmse.df <- perfs.methods.df %>%
  group_by(learner.id) %>%
  summarise(rmse.mean = mean(rmse.test.rmse))
perfs.methods.aggr.mae.df <- perfs.methods.df %>%
  group_by(learner.id) %>%
  summarise(mae.mean = mean(mae.test.mean))
perfs.methods.aggr.df <- left_join(perfs.methods.aggr.rmse.df, perfs.methods.aggr.mae.df, by = "learner.id")
# make learner id more readable for the graph
perfs.methods.aggr.df$learner.id <- substr(perfs.methods.aggr.df$learner.id, 
                                           start = 1, 
                                           stop = nchar(as.character(perfs.methods.aggr.df$learner.id))-9)
# visualize the performances
ggplot(perfs.methods.aggr.df, aes(y = reorder(learner.id, -mae.mean, sum))) +
  geom_point(aes(x = rmse.mean, color = "RMSE")) +
  geom_point(aes(x = mae.mean, color = "MAE")) +
  scale_x_continuous(breaks = seq(0.7, 1.3, by = 0.1), limits = c(0.7, 1.2)) +
  labs(x = "Values", y = "Learners", color = "Error") +
  ggtitle("Comparison of methods")

# get coefficents from regression model equation
data.stations.n.df <- data.stations.n.df %>%
  mutate(regr.model = purrr::map(
    tasks,
    train,
    learner = lrns.l[[4]]
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
  # equation for the hour
  models.df$Equation[i] <- paste0("T = ", round(as.numeric(data.stations.n.df$get.model[[i]][['learner.model']][['coefficients']][['(Intercept)']]), digits = 6),
                                  " + ", round(as.numeric(data.stations.n.df$get.model[[i]][['learner.model']][['coefficients']][[2]]), digits = 6),
                                  ".", names(data.stations.n.df$get.model[[i]][['learner.model']][['coefficients']])[[2]],
                                  " + ", round(as.numeric(data.stations.n.df$get.model[[i]][['learner.model']][['coefficients']][[3]]), digits = 6),
                                  ".", names(data.stations.n.df$get.model[[i]][['learner.model']][['coefficients']])[[3]]
  )
  # explanatory variables used for the model
  models.df$BestVar1[i] <- names(data.stations.n.df$get.model[[i]][['learner.model']][['coefficients']])[[2]]
  models.df$BestVar2[i] <- names(data.stations.n.df$get.model[[i]][['learner.model']][['coefficients']])[[3]]
  
  # error associated to the model
  models.df$RMSE <- perfs.methods.2bestsVar.df$rmse.test.rmse
  models.df$MAE <- perfs.methods.2bestsVar.df$mae.test.mean
}

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
# load dssf records from the period you want
# data prepared from geojson available on FTP, see prepare_dssf_2y.R in explorations/solar_irradiance
# one file for each period of 6 months named dssf.yymmdd_yymmdd.df
load("./data/dssf.180101_180630.df.rda")
dssf.n.df <- dssf.180101_180630.df %>%
  group_by(mhour) %>%
  nest()
# spatialize dssf for the hour you want, function in explorations/solar_irradiance/R
dssf.pred.df <- build.dssf.hour(dssf.n.df, "2018-05-02 14:00:00", grid.1000.pt.sp)
# add ens variable to prediction grid
expl.static.grid.df$ens <- dssf.pred.df$ens.pred
# spatialize temperature on the grid
interpolated.tsa.sf <- spatialize(regr.lrn = lrns.l[[4]],
                            task = data.stations.n.df$tasks[[which(data.stations.n.df$mtime == '2018-05-02 14:00:00')]],
                            prediction_grid.df = expl.static.grid.df,
                            predict.type = "se"
                            ) %>%
  dplyr::select(gid, geometry, response, se)

# get Wallonia boundaries
boundaries.sp <- raster::getData('GADM', country="BE", level=1, path = "./external-data/Boundaries") %>%
  subset(NAME_1 == "Wallonie")
boundaries.sp <- spTransform(boundaries.sp, CRSobj = lambert2008.crs)
boundaries.sf <- st_as_sf(boundaries.sp)
st_crs(boundaries.sf) <- lambert2008.crs

# prepare data to be visualized (transform from points to grid and then storing them in a df)
interpolated.tsa.sp <- as(interpolated.tsa.sf, "Spatial") %>%
  as(., "SpatialPixelsDataFrame") %>%
  as(., "SpatialGridDataFrame")
interpolated.tsa.df<- as.data.frame(interpolated.tsa.sp)

# build the map
ggmap <- build.static.ggmap(gridded.data.df = interpolated.tsa.df,
                            boundaries.sf = boundaries.sf,
                            layer.error.bool = T,
                            legend.error.bool = F,
                            pretty_breaks.bool = T,
                            title.chr = "Interpolated temperature with multiple linear regression - 2 bests variables",
                            legend.chr = "Temperature (°C)",
                            target.chr = "response",
                            target.name = "temp",
                            nb_classes.num = 10,
                            reverse_pal.bool = T,
                            resolution.chr = "Resolution : 1 km²")
ggmap

