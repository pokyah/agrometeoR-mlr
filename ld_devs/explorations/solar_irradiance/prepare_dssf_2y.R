
### preparation dssf from 2015-11-11 to 2018-06-30
library(jsonlite)
library(agrometAPI)
dssf.pt1.l <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/wallonia-dssfFm2015-11-11To2015-12-31.geojson")
dssf.pt2.l <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/wallonia-dssfFm2016-01-01To2016-06-30.geojson")
dssf.pt3.l <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/wallonia-dssfFm2016-07-01To2016-12-31.geojson")
dssf.pt4.l <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/wallonia-dssfFm2017-01-01To2017-06-30.geojson")
dssf.pt5.l <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/wallonia-dssfFm2017-07-01To2017-12-31.geojson")
dssf.pt6.l <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/wallonia-dssfFm2018-01-01To2018-06-30.geojson")
dssf.pt1.df <- dssf.pt1.l$features
dssf.pt2.df <- dssf.pt2.l$features
dssf.pt3.df <- dssf.pt3.l$features
dssf.pt4.df <- dssf.pt4.l$features
dssf.pt5.df <- dssf.pt5.l$features
dssf.pt6.df <- dssf.pt6.l$features
date_ens.pt1.l <- dssf.pt1.df$properties
date_ens.pt1.l <- purrr::imap(date_ens.pt1.l, function(x, sid) cbind(x, sid))
coords.pt1.l <- dssf.pt1.df$geometry$coordinates
coords.pt1.l <- lapply(coords.pt1.l, function(x) as.data.frame(t(x)))
coords.pt1.l <- purrr::imap(coords.pt1.l, function(x, sid) cbind(x, sid))
dssf.pt1.df <- purrr::map2_df(date_ens.pt1.l, coords.pt1.l, dplyr::left_join, by="sid")
colnames(dssf.pt1.df) <- c("ens", "mhour", "sid", "lat", "lon")
date_ens.pt2.l <- dssf.pt2.df$properties
date_ens.pt2.l <- purrr::imap(date_ens.pt2.l, function(x, sid) cbind(x, sid))
coords.pt2.l <- dssf.pt2.df$geometry$coordinates
coords.pt2.l <- lapply(coords.pt2.l, function(x) as.data.frame(t(x)))
coords.pt2.l <- purrr::imap(coords.pt2.l, function(x, sid) cbind(x, sid))
dssf.pt2.df <- purrr::map2_df(date_ens.pt2.l, coords.pt2.l, dplyr::left_join, by="sid")
colnames(dssf.pt2.df) <- c("mhour", "ens", "sid", "lat", "lon")
date_ens.pt3.l <- dssf.pt3.df$properties
date_ens.pt3.l <- purrr::imap(date_ens.pt3.l, function(x, sid) cbind(x, sid))
coords.pt3.l <- dssf.pt3.df$geometry$coordinates
coords.pt3.l <- lapply(coords.pt3.l, function(x) as.data.frame(t(x)))
coords.pt3.l <- purrr::imap(coords.pt3.l, function(x, sid) cbind(x, sid))
dssf.pt3.df <- purrr::map2_df(date_ens.pt3.l, coords.pt3.l, dplyr::left_join, by="sid")
colnames(dssf.pt3.df) <- c("ens", "mhour", "sid", "lat", "lon")
date_ens.pt4.l <- dssf.pt4.df$properties
date_ens.pt4.l <- purrr::imap(date_ens.pt4.l, function(x, sid) cbind(x, sid))
coords.pt4.l <- dssf.pt4.df$geometry$coordinates
coords.pt4.l <- lapply(coords.pt4.l, function(x) as.data.frame(t(x)))
coords.pt4.l <- purrr::imap(coords.pt4.l, function(x, sid) cbind(x, sid))
dssf.pt4.df <- purrr::map2_df(date_ens.pt4.l, coords.pt4.l, dplyr::left_join, by="sid")
colnames(dssf.pt4.df) <- c("mhour", "ens", "sid", "lat", "lon")
date_ens.pt5.l <- dssf.pt5.df$properties
date_ens.pt5.l <- purrr::imap(date_ens.pt5.l, function(x, sid) cbind(x, sid))
coords.pt5.l <- dssf.pt5.df$geometry$coordinates
coords.pt5.l <- lapply(coords.pt5.l, function(x) as.data.frame(t(x)))
coords.pt5.l <- purrr::imap(coords.pt5.l, function(x, sid) cbind(x, sid))
dssf.pt5.df <- purrr::map2_df(date_ens.pt5.l, coords.pt5.l, dplyr::left_join, by="sid")
colnames(dssf.pt5.df) <- c("mhour", "ens", "sid", "lat", "lon")
date_ens.pt6.l <- dssf.pt6.df$properties
date_ens.pt6.l <- purrr::imap(date_ens.pt6.l, function(x, sid) cbind(x, sid))
coords.pt6.l <- dssf.pt6.df$geometry$coordinates
coords.pt6.l <- lapply(coords.pt6.l, function(x) as.data.frame(t(x)))
coords.pt6.l <- purrr::imap(coords.pt6.l, function(x, sid) cbind(x, sid))
dssf.pt6.df <- purrr::map2_df(date_ens.pt6.l, coords.pt6.l, dplyr::left_join, by="sid")
colnames(dssf.pt6.df) <- c("mhour", "ens", "sid", "lat", "lon")
stations_meta.df <- NULL
dssf.151111_151231.df <- prepare_agromet_API_data.fun(list(stations_meta.df, dssf.pt1.df), "get_rawdata_dssf")
dssf.160101_160630.df <- prepare_agromet_API_data.fun(list(stations_meta.df, dssf.pt2.df), "get_rawdata_dssf")
dssf.160701_161231.df <- prepare_agromet_API_data.fun(list(stations_meta.df, dssf.pt3.df), "get_rawdata_dssf")
dssf.170101_170630.df <- prepare_agromet_API_data.fun(list(stations_meta.df, dssf.pt4.df), "get_rawdata_dssf")
dssf.170701_171231.df <- prepare_agromet_API_data.fun(list(stations_meta.df, dssf.pt5.df), "get_rawdata_dssf")
dssf.180101_180630.df <- prepare_agromet_API_data.fun(list(stations_meta.df, dssf.pt6.df), "get_rawdata_dssf")
dssf.151111_151231.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.151111_151231.df$ens)))
dssf.160101_160630.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.160101_160630.df$ens)))
dssf.160701_161231.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.160701_161231.df$ens)))
dssf.170101_170630.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.170101_170630.df$ens)))
dssf.170701_171231.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.170701_171231.df$ens)))
dssf.180101_180630.df$ens <- as.numeric(gsub(",", ".", gsub("\\.", "", dssf.180101_180630.df$ens)))

dssf.151111_151231.n.df <- dssf.151111_151231.df %>%
  group_by(mhour) %>%
  nest()
dssf.160101_160630.n.df <- dssf.160101_160630.df %>%
  group_by(mhour) %>%
  nest()
dssf.160701_161231.n.df <- dssf.160701_161231.df %>%
  group_by(mhour) %>%
  nest()
dssf.170101_170630.n.df <- dssf.170101_170630.df %>%
  group_by(mhour) %>%
  nest()
dssf.170701_171231.n.df <- dssf.170701_171231.df %>%
  group_by(mhour) %>%
  nest()
dssf.180101_180630.n.df <- dssf.180101_180630.df %>%
  group_by(mhour) %>%
  nest()

save("dssf.151111_151231.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.151111_151231.df.rda")
save("dssf.160101_160630.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.160101_160630.df.rda")
save("dssf.160701_161231.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.160701_161231.df.rda")
save("dssf.170101_170630.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.170101_170630.df.rda")
save("dssf.170701_171231.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.170701_171231.df.rda")
save("dssf.180101_180630.df", file = "~/Documents/code/agrometeoR-mlr/data/dssf.180101_180630.df.rda")




