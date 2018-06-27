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
#' @param lsa.json A JSON file with records from API
#' @param hour.chr A character specifying which hour you want ; respect the format 'YYYY-MM-DD HH:MM:SS'
#' @param grid.pt.sp A SpatialPointsDataFrame grid with centroids
#' @return a dataframe of spatialized solar irradiance for an hour
#' @export
build.lsa.hour.df <- function(lsa.json, hour.chr, grid.pt.sp) {
  
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
      lsa.litest_solar <- get_from_agromet_API.fun(
  user_token.chr=Sys.getenv("AGROMET_API_V1_KEY"),
  table_name.chr="get_rawdata_dssf",
  sensors.chr=NULL,
  stations_ids.chr=NULL,
  dfrom.chr="2018-06-10",
  dto.chr="2018-06-11",
  month_day.chr=NULL,
  api_v.chr="v2",
  test.bool=FALSE
)st[[i]][j, 'long'] <- lsa.coordinates.list[[i]][1]
      lsa.list[[i]][j, 'lat'] <- lsa.coordinates.list[[i]][2]
      lsa.list[[i]][j, 'sid'] <- paste0("st_",i)
    }
  }
  
  # create a data frame with every data
  lsa.df <- bind_rows(lsa.list)
  lsa.df$ens <- as.numeric(lsa.df$ens)
  lsa.df$mhour <- lubridate::as_datetime(lsa.df$mhour) # date with format 'YYYY-MM-DD HH:MM:SS'
  
  lsa.nested.df <- lsa.df %>%
    group_by(mhour) %>%
    tidyr::nest()
  
  lsa.hour.df <- subset(lsa.nested.df, mhour == hour.chr)
  
  lsa.hour.sp <- lsa.hour.df$data[[1]]             
  sp::coordinates(lsa.hour.sp) <- ~long+lat
  # set crs lambert 2008
  raster::crs(lsa.hour.sp) <- "+proj=longlat +datum=WGS84 +no_defs"
  lsa.hour.sp <- sp::spTransform(lsa.hour.sp,
                             CRSobj ="+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # interpolate surface with IDW method (inverse distance weighted)
  idw <- gstat::idw(formula = ens ~ 1, locations = lsa.hour.sp, newdata = grid.pt.sp)  # apply idw model for the data
  idw.output <- as.data.frame(idw) %>% # output is defined as a data table
    dplyr::select(-var1.var)
  names(idw.output)[1:3] <- c("long", "lat", "ens.pred")  # give names to the modelled variables

  return(idw.output)
}

# load json file with solar irradiance records
lsa.json <- jsonlite::fromJSON("~/Documents/code/agromet-tests/data-raw/lsasaf.json")
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

lsa.20180611_14.df <- build.lsa.hour.df(
  lsa.json = lsa.json,
  hour.chr = "2018-06-11 14:00:00",
  grid.pt.sp = grid.1000.pt.sp
)
lsa.20180611_14.sf <- sf::st_as_sf(lsa.20180611_14.df, coords = c("long", "lat"))
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
st_crs(lsa.20180611_14.sf) <- lambert2008.crs
plot(lsa.20180611_14.sf)

lsa.20180611_15.df <- get.hourly_lsa.wallonia.df(
  lsa.json =test_solar <- get_from_agromet_API.fun(
  user_token.chr=Sys.getenv("AGROMET_API_V1_KEY"),
  table_name.chr="get_rawdata_dssf",
  sensors.chr=NULL,
  stations_ids.chr=NULL,
  dfrom.chr="2018-06-10",
  dto.chr="2018-06-11",
  month_day.chr=NULL,
  api_v.chr="v2",
  test.bool=FALSE
) lsa.json,
  hour.chr = "2018-06-11 15:00:00",
  grid.pt.sp = grid.1000.pt.sp
)

# virtual stations with their solar irradiance for an hour
vs.grid.ens.20180611_14.sf <- st_join(grid.1000.pt.sf, lsa.20180611_14.sf)

# load physical stations locations
ps.locations.points_sf <- build.ps.locations.points_sf.fun(sf.bool = TRUE, EPSG.chr = 3812)

# https://www.neonscience.org/field-data-polygons-centroids
# set the radius for the plots
radius <- 500 # radius in meters
# define thetest_solar <- get_from_agromet_API.fun(
  user_token.chr=Sys.getenv("AGROMET_API_V1_KEY"),
  table_name.chr="get_rawdata_dssf",
  sensors.chr=NULL,
  stations_ids.chr=NULL,
  dfrom.chr="2018-06-10",
  dto.chr="2018-06-11",
  month_day.chr=NULL,
  api_v.chr="v2",
  test.bool=FALSE
) plot edges based upon the plot radius. 
yPlus <- lsa.20180611_14.sf$northing+radius
xPlus <- lsa.20180611_14.sf$easting+radius
yMinus <- lsa.20180611_14.sf$northing-radius
xMinus <- lsa.20180611_14.sf$easting-radius
# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close polygon
# Extract the plot ID information
sid=lsa.20180611_14.sf$sid

# create spatial polygons from coordinates
polys <- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), sid=id)
}, 
split(square, row(square)), sid),
proj4string=CRS(lambert2008.crs))





ps.locations.ens.20180611_14.points_sf <- st_intersection(st_buffer(ps.locations.points_sf, dist = 500), lsa.20180611_14.sf)


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


