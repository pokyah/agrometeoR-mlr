#' Spatialize solar irradiance for an hour and return its result in a dataframe
#' @author Loïc Davadan - ldavadan.github.io
#' @param dfrom.chr A character which specifies the date from which you want to make a benchmark ; respect the format 'YYYY-MM-DD'
#' @param dto.chr A character which specifies the date until which you want to make a benchmark ; respect the format 'YYYY-MM-DD'. Warning ! You will have only the result of midnight for this day
#' @return a dataframe of spatialized solar irradiance for an hour
#' @export
make.benchmark_learners <- function(dfrom.chr, dto.chr){
  
  # Retrieving from API
  records.stations.df <- prepare_agromet_API_data.fun(
    get_from_agromet_API.fun(
      user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
      table_name.chr = "cleandata",
      stations_ids.chr = "all",
      sensors.chr = "tsa,ens",
      dfrom.chr = dfrom.chr,
      dto.chr = dto.chr,
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
    filter(mtime >= paste0(dfrom.chr, " 00:00:00") & mtime <= paste0(dto.chr, " 23:00:00"))
  
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
    keep.pred = TRUE,
    show.info = TRUE,
    models = FALSE,
    measures = list(mse, timetrain)
  )
  
  return(bmr.l)
}

