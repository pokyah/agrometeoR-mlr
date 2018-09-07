
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


data.dyn.stations.df <- records.stations.df %>%
  group_by(mtime) %>%
  nest()


# converting each tibble of the nested records to a strict dataframe (required by mlr)
# ::todo:: need to use transmute_at
data.dyn.stations.df <- data.dyn.stations.df %>%
  mutate(data_as_df = purrr::map(
    .x = data,
    .f = data.frame
  ))

# removing the tibbles columns and only keeping the pure dataframes (required by mlr)
data.dyn.stations.df <- data.dyn.stations.df %>%
  dplyr::select(mtime, data_as_df)



# defining the regression tasks on the stations observations for each of the hourly datasets
# https://stackoverflow.com/questions/46868706/failed-to-use-map2-with-mutate-with-purrr-and-dplyr
# https://stackoverflow.com/questions/42518156/use-purrrmap-to-apply-multiple-arguments-to-a-function?rq=1
data.dyn.stations.df <- data.dyn.stations.df %>%
  mutate(tasks = purrr::map2(
    as.character(mtime),
    data_as_df,
    mlr::makeRegrTask,
    target = target.chr
  )
  )

expl.static.stations.df <- expl.static.stations.df %>%
  dplyr::select(-geometry) %>%
  dplyr::select(-X) %>%
  dplyr::select(-Y)


bmr.l <- benchmark(
  learners = lrns.l,
  tasks = data.dyn.stations.df$tasks,
  resamplings = resampling.l,
  keep.pred = FALSE,
  show.info = TRUE,
  models = FALSE,
  measures = list(mse, timetrain)
)









