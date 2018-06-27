test_solar <- get_from_agromet_API.fun(
  user_token.chr=Sys.getenv("AGROMET_API_V1_KEY"),
  table_name.chr="get_rawdata_dssf",
  sensors.chr=NULL,
  stations_ids.chr=NULL,
  dfrom.chr="2018-06-10",
  dto.chr="2018-06-11",
  month_day.chr=NULL,
  api_v.chr="v2",
  test.bool=FALSE
)

