# Retrieving from API
records.stations.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "all",
    sensors.chr = "tsa,ens,hra, hct",
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
  filter(!is.na(ens)) %>%
  filter (!is.na(hra)) %>%
  filter(!is.na(hct))

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

data.stations.n.df <- make.benchmark.tasks(static.vars = expl.static.stations.sf,
                                         dynamic.vars = records.stations.sf,
                                         target.chr = "tsa")

# defining the learners who will be compared
lrns.l <- makeLearners(cls = c("lm", "elmNN", "kknn"),
                       ids = c("linear regression", "single layer neural net", "nearest neighbours"),
                       type = "regr"
)

# defining the validation (resampling) strategy
resampling.l = mlr::makeResampleDesc(
  method = "LOO"#,
  #predict = "test"
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
  keep.pred = TRUE,
  show.info = TRUE,
  models = FALSE,
  measures = list(mse, timetrain)
)

perfs.methods.df <- getBMRAggrPerformances(bmr.l,as.df = TRUE)
tsa.predict.df <- getBMRPredictions(bmr.l, as.df = TRUE)

plotBMRBoxplots(bmr.l, measure = mse, style = "violin")
plotBMRSummary(bmr.l)
plotBMRRanksAsBarChart(bmr.l, pos = "dodge", order.lrn = getBMRLearnerIds(bmr.l))

global.methods.perfs <- perfs.methods.df %>%
  group_by(learner.id) %>%
  dplyr::summarise(global = mean(mse.test.mean))

# extracting the best learner from the bmr.l
# ::todo:: combination of tsaks
best.method <- getBMRLearners(bmr.l)[names(getBMRLearners(bmr.l)) == filter(global.methods.perfs, global == min(global))$learner.id]

# defining the standard error learner by altering the previous one.
# We need it to make a map that combines prediction with uncertainty
se.regr.lrn = setPredictType(lrns.l, "se")

fv <- purrr::map(
  data.stations.n.df$tasks,
  .f = mlr::generateFilterValuesData,
  method = "linear.correlation"
  )

filtered.tasks <-  purrr::map(
    data.stations.n.df$tasks,
    mlr::filterFeatures,
    method = "linear.correlation",
    perc = 0.25
  )


bmr.l2 <- benchmark(learners = lrns.l,
                    tasks = filtered.tasks,
                    resamplings = resampling.l,
                    measures = list(mse, timetrain),
                    keep.pred = TRUE,
                    show.info = TRUE,
                    models = FALSE
                    )








