# Avoid interference with old variables by cleaning the Global Environment
rm(list=ls(all=TRUE))

# Dynamic Sourcing of all the required functions
source(paste0("./R/file_management.R"))
source_files_recursively.fun("./R")

library(tidyverse)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tidyr)
library(sp)
library(raster)
library(gstat)


#' Spatialize solar irradiance for an hour and return its result in a dataframe
#' @author Loïc Davadan - ldavadan.github.io
#' @param lsa.records.df A JSON file with records from API
#' @param hour.chr A character specifying which hour you want ; respect the format 'YYYY-MM-DD HH:MM:SS'
#' @param grid.pt.sp A SpatialPointsDataFrame grid with centroids
#' @return a dataframe of spatialized solar irradiance for an hour
#' @export
build.lsa.hour.df <- function(lsa.records.df, hour.chr, grid.pt.sp) {
  
  lsa.nested.df <- lsa.records.df %>%
    group_by(mhour) %>%
    tidyr::nest()
  
  lsa.hour.df <- subset(lsa.nested.df, mhour == hour.chr)
  
  lsa.hour.sp <- lsa.hour.df$data[[1]]             
  sp::coordinates(lsa.hour.sp) <- ~lat+lon    ########## err : inverse lat-lon ###########
  # set crs lambert 2008
  raster::crs(lsa.hour.sp) <- "+proj=longlat +datum=WGS84 +no_defs"
  lsa.hour.sp <- sp::spTransform(lsa.hour.sp,
                             CRSobj ="+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # interpolate surface with IDW method (inverse distance weighted)
  idw <- gstat::krige(formula = ens ~ 1, locations = lsa.hour.sp, newdata = grid.pt.sp)  # apply idw model for the data
  idw.output <- as.data.frame(idw) %>% # output is defined as a data table
    dplyr::select(-var1.var)
  names(idw.output)[1:3] <- c("long", "lat", "ens.pred")  # give names to the modelled variables

  return(idw.output)
}

# load solar irradiance data from API
test_solar <- prepare_agromet_API_data.fun(get_from_agromet_API.fun(
  user_token.chr=Sys.getenv("AGROMET_API_V1_KEY"),
  table_name.chr="get_rawdata_dssf",
  sensors.chr=NULL,
  stations_ids.chr=NULL,
  dfrom.chr="2018-06-11",
  dto.chr="2018-06-12",
  month_day.chr=NULL,
  api_v.chr="v2",
  test.bool=FALSE
), "get_rawdata_dssf")

# Create the virtual stations interpolation grid points
grid.1000.pt.sf <- build.vs.grid.fun(
  res.num = 1000,
  geom.chr = "centers",
  sf.bool = TRUE,
  EPSG.chr = "3812",
  country_code.chr = "BE",
  NAME_1.chr = "Wallonie"
)
grid.1000.pt.sp <- as(grid.1000.pt.sf, "Spatial")

# get spatialized lsa data for one hour
lsa.20180611_14.df <- build.lsa.hour.df(
  lsa.records.df = test_solar,
  hour.chr = "2018-06-11 14:00:00",
  grid.pt.sp = grid.1000.pt.sp
)
lsa.20180611_14.sf <- sf::st_as_sf(lsa.20180611_14.df, coords = c("long", "lat"))
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
st_crs(lsa.20180611_14.sf) <- lambert2008.crs

# get spatialized lsa data for one other hour to valid they have the same number of points
lsa.20180611_15.df <- build.lsa.hour.df(
  lsa.records.df = test_solar,
  hour.chr = "2018-06-11 15:00:00",
  grid.pt.sp = grid.1000.pt.sp
)

# get hourly solar irradiance for virtual stations (value for the centroid)
vs.grid.ens.20180611_14.pt.sf <- st_join(grid.1000.pt.sf, lsa.20180611_14.sf)
plot(vs.grid.ens.20180611_14.pt.sf[5])


# Create the virtual stations interpolation grid polygons (1 km² cells)
grid.1000.pg.sf <- build.vs.grid.fun(
  country_code.chr = "BE",
  NAME_1.chr = "Wallonie",
  res.num = 1000,
  geom.chr = "polygons",
  sf.bool = TRUE,
  EPSG.chr = 3812
)

# get hourly solar irradiance for virtual stations (same value for each coordinates in the polygon)
vs.grid.ens.20180611_14.pg.sf <- sf::st_join(grid.1000.pg.sf, vs.grid.ens.20180611_14.pt.sf) %>%
  dplyr::select(ISO.x, NAME_0.x, NAME_1.x, sid.x, ens.pred, geometry)
colnames(vs.grid.ens.20180611_14.pg.sf) <- c("ISO", "Country", "Region", "sid", "ens.pred", "geometry")
plot(vs.grid.ens.20180611_14.pg.sf[5])

# load physical stations locations
ps.locations.points_sf <- build.ps.locations.points_sf.fun(sf.bool = TRUE, EPSG.chr = 3812)

# get solar irradiance for each physical station
ps.locations.ens.20180611_14.pt_sf <- st_intersection(ps.locations.points_sf, vs.grid.ens.20180611_14.pg.sf) %>%
  dplyr::select(sid, altitude, ISO, Country, Region, ens.pred, geometry)
plot(ps.locations.ens.20180611_14.pt_sf[6])


