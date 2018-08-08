
load("./data/records.stations.df.rda")
load("./data/dssf.151111_151231.df.rda")
load("./data/dssf.160101_160630.df.rda")
load("./data/dssf.160701_161231.df.rda")
load("./data/dssf.170101_170630.df.rda")
load("./data/dssf.170701_171231.df.rda")
load("./data/dssf.180101_180630.df.rda")
load("./data/stations_meta.df.rda")

# filter stations meta
stations_meta.df <- stations_meta.df %>%
  dplyr::select(sid, poste, longitude, latitude, altitude, network_name, state, type_name) %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(state == "Ok") %>%
  filter(sid != 36) %>%
  dplyr::select(sid, longitude, latitude)
stations_meta.df$longitude <- as.numeric(stations_meta.df$longitude)
stations_meta.df$latitude <- as.numeric(stations_meta.df$latitude)

# reduce size records deleting column
records.stations.df <- records.stations.df %>% dplyr::select(-tsa)

# sf stations meta
stations_meta.sf <- st_as_sf(stations_meta.df, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

# filter with nearest points from stations
dssf.151111_151231.df <- dssf.151111_151231.df %>%
  filter(sid %in% c(875, 840, 838, 756, 746, 705, 711, 644, 624, 580,
                    552, 476, 465, 499, 396, 443, 307, 378, 330, 259,
                    247, 239, 254, 180, 154, 58, 132, 77, 62))
dssf.160101_160630.df <- dssf.160101_160630.df %>%
  filter(sid %in% c(875, 840, 838, 756, 746, 705, 711, 644, 624, 580,
                    552, 476, 465, 499, 396, 443, 307, 378, 330, 259,
                    247, 239, 254, 180, 154, 58, 132, 77, 62))
dssf.160701_161231.df <- dssf.160701_161231.df %>%
  filter(sid %in% c(875, 840, 838, 756, 746, 705, 711, 644, 624, 580,
                    552, 476, 465, 499, 396, 443, 307, 378, 330, 259,
                    247, 239, 254, 180, 154, 58, 132, 77, 62))
dssf.170101_170630.df <- dssf.170101_170630.df %>%
  filter(sid %in% c(875, 840, 838, 756, 746, 705, 711, 644, 624, 580,
                    552, 476, 465, 499, 396, 443, 307, 378, 330, 259,
                    247, 239, 254, 180, 154, 58, 132, 77, 62))
dssf.170701_171231.df <- dssf.170701_171231.df %>%
  filter(sid %in% c(875, 840, 838, 756, 746, 705, 711, 644, 624, 580,
                    552, 476, 465, 499, 396, 443, 307, 378, 330, 259,
                    247, 239, 254, 180, 154, 58, 132, 77, 62))
dssf.180101_180630.df <- dssf.180101_180630.df %>%
  filter(sid %in% c(875, 840, 838, 756, 746, 705, 711, 644, 624, 580,
                    552, 476, 465, 499, 396, 443, 307, 378, 330, 259,
                    247, 239, 254, 180, 154, 58, 132, 77, 62))

# # sf dssf records
# dssf.151111_151231.sf <- st_as_sf(dssf.151111_151231.df, coords = c("lat", "lon"), crs = "+proj=longlat +datum=WGS84 +no_defs")
# dssf.160101_160630.sf <- st_as_sf(dssf.160101_160630.df, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
# dssf.160701_161231.sf <- st_as_sf(dssf.160701_161231.df, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
# dssf.170101_170630.sf <- st_as_sf(dssf.170101_170630.df, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
# dssf.170701_171231.sf <- st_as_sf(dssf.170701_171231.df, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
# dssf.180101_180630.sf <- st_as_sf(dssf.180101_180630.df, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")

# # check nearest points from stations
# dssf.sid.sf <- dssf.151111_151231.sf %>%
# select(sid, geometry)
# dssf.sid.sf <- dssf.sid.sf[!duplicated(dssf.sid.sf),]
# mapview(dssf.sid.sf) + stations_meta.sf


dssf.records.df <- bind_rows(dssf.151111_151231.df, dssf.160101_160630.df) %>%
  bind_rows(., dssf.160701_161231.df) %>%
  bind_rows(., dssf.170101_170630.df) %>%
  bind_rows(., dssf.170701_171231.df) %>%
  bind_rows(., dssf.180101_180630.df)


table.stations.df <- as.data.frame(table(records.stations.df$mtime))
table.stations.df <- table.stations.df[table.stations.df$Freq == 29,]
table.dssf.df <- table(dssf.records.df$mhour)

dssf.records.df <- dssf.records.df %>%
  filter(mhour >= "2016-01-01 00:00:00" & mhour <= "2018-06-01 00:00:00") %>%
  filter(mhour != "2017-12-31" & mhour != "2017-06-30") %>%
  filter(mhour %in% as.POSIXct(table.stations.df$Var1))
records.stations.df <- records.stations.df %>%
  filter(mtime >= "2016-01-01 00:00:00" & mtime <= "2018-06-01 00:00:00") %>%
  filter(mtime %in% dssf.records.df$mhour)

order <- c(756, 840, 838, 711, 705, 644, 443, 476, 624, 58, 307, 552, 499, 396, 
           254, 247, 875, 132, 259, 77, 239, 378, 465, 154, 62, 180, 746, 330, 580)
dssf.records.df <- dssf.records.df[match(order, dssf.records.df$sid),]
# couple correlation
cor(dssf.records.df$ens, records.stations.df$ens, use = "na.or.complete")
correlations.df <- data.frame(couple.id_station.id_pix = c('25/396', '23/552', '24/499', '42/330', '14/443', '35/378', '33/77',
                                         '30/132', '32/259', '26/254', '39/62', '18/58', '40/180', '27/247',
                                         '19/307', '34/239', '38/154', '37/465', '15/476', '13/644', '61/580',
                                         '17/624', '41/746', '10/705', '9/711', '1/756', '4/840', '7/838',
                                         '29/875'),
                              correlation = c(0.9557991, 0.953246, 0.9272308, 0.955108, 0.945988,
                                              0.9517486, 0.9555424, 0.9575363, 0.9578885, 0.958013,
                                              0.9547781, 0.9583265, 0.9475031, 0.9440548, 0.954955,
                                              0.956161, 0.962355, 0.9422871, 0.9514156, 0.958661,
                                              0.9343718, 0.9578254, 0.9301199, 0.9562283, 0.9510065,
                                              0.9438328, 0.9581045, 0.9515559, 0.9614241))
mean(correlations.df$correlation)
