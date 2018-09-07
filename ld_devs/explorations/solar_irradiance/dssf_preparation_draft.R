# Dynamic Sourcing of all the required functions
source(paste0("./R/file_management.R"))
source_files_recursively.fun("./R")
source_files_recursively.fun("./ld_devs/explorations/solar_irradiance/R")

library(jsonlite)

# load JSON file
lsa.json <- jsonlite::fromJSON("~/Documents/code/agrometeoR-mlr/data/wallonia-dssfFm2018-03-01To2018-05-31.geojson")

# get data (date and ens) in a list
lsa.list <- lsa.json$features$properties

# get coordinates in a list
lsa.coordinates.list <- lsa.json$features$geometry$coordinates

# add coordinates of each measure
for(i in 1:length(lsa.coordinates.list)){
  lsa.list[[i]][,'long'] <- NA
  lsa.list[[i]][,'lat'] <- NA
  lsa.list[[i]][,'sid'] <- NA
}
for(i in 1:length(lsa.coordinates.list)){
  for(j in 1:nrow(lsa.list[[i]])){
    lsa.list[[i]][j, 'long'] <- lsa.coordinates.list[[i]][1]
    lsa.list[[i]][j, 'lat'] <- lsa.coordinates.list[[i]][2]
    lsa.list[[i]][j, 'sid'] <- paste0("st_",i)
  }
}

# create a data frame with every data
lsa.df <- bind_rows(lsa.list)
lsa.df$ens <- as.numeric(lsa.df$ens)
lsa.df$mhour <- as_datetime(lsa.df$mhour)

lsa.df <- lsa.df %>%
  filter(!is.nan(ens) == TRUE)

lsa.nested.df <- lsa.df %>%
  group_by(mhour) %>%
  nest()


# lsa.20180312_14.sf <- st_as_sf(data.frame(lsa.nested.df$data[which(lsa.nested.df$mhour == "2018-03-12 14:00:00")]), coords = c("long", "lat"))
# st_crs(lsa.20180312_14.sf) <- "+proj=longlat +datum=WGS84 +no_defs"
# 
# # convert to Lambert 2008 CRS
# lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# lsa.20180312_14.sf <- st_transform(lsa.20180312_14.sf, crs = lambert2008.crs)
# lsa.20180312_14.sp <- as(lsa.20180312_14.sf, "Spatial")
# 
# grid.1000.pt.sp <- build.vs.grid.fun(
#   res.num = 1000,
#   geom.chr = "centers",
#   sf.bool = F,
#   EPSG.chr = "3812",
#   country_code.chr = "BE",
#   NAME_1.chr = "Wallonie"
# )
# 
# 
# lsa.20180502_14.df <- build.dssf.hour(
#   dssf.nested.df = lsa.nested.df,
#   hour.chr = "2018-05-02 14:00:00",
#   grid.pt.sp = grid.1000.pt.sp
# )
# lsa.20180502_14.sf <- st_as_sf(lsa.20180502_14.df, coords = c("long", "lat"))
# st_crs(lsa.20180502_14.sf) <- lambert2008.crs
# # plot(lsa.20180502_14.sf[3])
# 
# grid.dssf.pg.sf <- st_join(grid.1000.pg.sf, lsa.20180502_14.sf)
# plot(grid.dssf.pg.sf[5])
# 
# # load("./data/expl.static.stations.sf.rda")
# expl_ens.static.stations.sf <- st_intersection(expl.static.stations.sf, grid.dssf.pg.sf) %>%
#   dplyr::select(gid, altitude, slope, aspect, roughness, crops, artificial, forest, herbaceous, ens.pred, geometry)
# expl_ens.static.stations.df <- data.frame(dplyr::bind_cols(expl_ens.static.stations.sf, data.frame(sf::st_coordinates(expl_ens.static.stations.sf)))) %>%
#   dplyr::select(-geometry)



# spatialize data for each hour
lsa.nested.df <- lsa.nested.df %>%
  mutate(spatialized.data = purrr::map(
    data,
    build.hourly.spatialized.dssf.df,
    grid.1000.pt.sp
  ))

# lsa.nested.df <- lsa.nested.df %>%
#   dplyr::select(mhour, spatialized.data)

# # transform to sf
# lsa.nested.df <- lsa.nested.df %>%
#   mutate(spatialized.data.sf = purrr::map(
#     spatialized.data,
#     st_as_sf,
#     coords = c("long", "lat")
#   ))
# 
# lsa.nested.df <- lsa.nested.df %>%
#   dplyr::select(mhour, spatialized.data.sf)
# 
# # spatialize on a grid with 1 km² cells
# lsa.nested.df <- lsa.nested.df %>%
#   mutate(spatialized.grid.pg.sf = purrr::map(
#     spatialized.data.sf,
#     st_join,
#     grid.1000.pg.sf
#   ))
# 
# lsa.nested.df <- lsa.nested.df %>%
#   dplyr::select(mhour, spatialized.grid.pg.sf)
# 
# # intersect with physical stations
# lsa.nested.df <- lsa.nested.df %>%
#   mutate(stations.records.dssf.sf = purrr::map(
#     spatialized.grid.pg.sf,
#     st_intersection,
#     expl.static.stations.sf
#   ))


