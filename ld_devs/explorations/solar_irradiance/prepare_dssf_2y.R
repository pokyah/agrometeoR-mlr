
### preparation dssf from 2015-11-11 to 2018-06-30
library(jsonlite)
library(agrometAPI)

# function to prepare geojson
prepare.dssf <- function(path.geojson.chr){
  dssf.l <- jsonlite::fromJSON(path.geojson.chr)
  dssf.df <- dssf.l$features
  
  date_ens.l <- dssf.df$properties
  date_ens.l <- purrr::imap(date_ens.l, function(x, sid) cbind(x, sid))
  coords.l <- dssf.df$geometry$coordinates
  coords.l <- lapply(coords.l, function(x) as.data.frame(t(x)))
  coords.l <- purrr::imap(coords.l, function(x, sid) cbind(x, sid))
  dssf.df <- purrr::map2_df(date_ens.l, coords.l, dplyr::left_join, by="sid")
}

# ordering and renaming columns
periods <- c('Fm2015-11-11To2015-12-31', 'Fm2016-01-01To2016-06-30', 'Fm2016-07-01To2016-12-31',
             'Fm2017-01-01To2017-06-30', 'Fm2017-07-01To2017-12-31', 'Fm2018-01-01To2018-06-30')
path <- sprintf("~/Documents/code/agrometeoR-mlr/data/wallonia-dssf%s.geojson", periods)
dssf.pt.l <- lapply(path, prepare.dssf) %>%
  lapply(., function(X){X[c('mhour', 'ens', 'sid', 'V1', 'V2')]}) %>%
  lapply(., setNames, nm = c("mhour", "ens", "sid", "lat", "lon"))
stations_meta.df <- NULL

dssf.pt.l <- lapply(dssf.pt.l, function(X){
  prepare_agromet_API_data.fun(list(stations_meta.df, X), table_name ="get_rawdata_dssf")})

# naming objects from the list
names(dssf.pt.l) <- c('dssf.151111_151231.df', 'dssf.160101_160630.df', 'dssf.160701_161231.df',
                   'dssf.170101_170630.df', 'dssf.170701_171231.df', 'dssf.180101_180630.df')
# adding objects of list in environment
list2env(dssf.pt.l, envir = .GlobalEnv)

# correcting bugs about values
dssf.151111_151231.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.151111_151231.df$ens)))
dssf.160101_160630.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.160101_160630.df$ens)))
dssf.160701_161231.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.160701_161231.df$ens)))
dssf.170101_170630.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.170101_170630.df$ens)))
dssf.170701_171231.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.170701_171231.df$ens)))
dssf.180101_180630.df$ens <- as.numeric(dssf.180101_180630.df$ens)

save("dssf.151111_151231.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.151111_151231.df.rda")
save("dssf.160101_160630.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.160101_160630.df.rda")
save("dssf.160701_161231.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.160701_161231.df.rda")
save("dssf.170101_170630.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.170101_170630.df.rda")
save("dssf.170701_171231.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.170701_171231.df.rda")
save("dssf.180101_180630.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.180101_180630.df.rda")




