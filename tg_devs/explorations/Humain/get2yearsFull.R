source("./R/file_management.R")
source_files_recursively.fun("./R")
#+ ---------------------------------
#' ## Data Acquisition
#' In order to get the Pameseb data from the [API](https://app.pameseb.be/fr/), you need your own token to be stored in you [.Renviron file](https://csgillespie.github.io/efficientR/set-up.html#r-startup).
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
#+ ---------------------------------
# get the list of all Pameseb active stations to populate the 2 stations inputs.
stations.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr= Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "station",
    stations_ids.chr = "all"
  ),"station"
)
#+ ---------------------------------
# Get the Humain Pameseb station (sid=61)
records_pameseb.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "61",
    sensors.chr = "tsa, ens, vvt, plu, sunrise, sunset",
    dfrom.chr = "2015-11-01",
    dto.chr = "2017-11-01",
    api_v.chr = "v2"
  ),"cleandata"
)
#+ ---------------------------------
# Get the Humain IRM station (sid=1000)
records_irm_df <- prepare_agromet_API_data.fun(get_from_agromet_API.fun(
  user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
  table_name.chr = "get_rawdata_irm",
  stations_ids.chr = "1000",
  sensors.chr = "tsa, ens, vvt, plu, hra, sunrise, sunset",
  dfrom.chr = "2015-11-01",
  dto.chr = "2017-11-01",
  api_v.chr = "v2"
), "cleandata")
#+ ---------------------------------
# Combine the 2 stations datasets in a single dataset for easier data manipulation
records.df <- h.filter_records(
  records.df = bind_rows(records_pameseb.df, records_irm_df),
  sensor.chr = "tsa",
  dateRange.chr = c(as.Date("2015-11-01"),as.Date("2017-11-01")),
  filter.chr = "no_extra_filter"
)
cat(paste0("Your dataset contains ", nrow(records.df)/2, " hourly records"))
number_of_records.num <- nrow(records.df)/2
