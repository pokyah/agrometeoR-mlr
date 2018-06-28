#'---
#'author: "Thomas Goossens (CRA-W) - hello.pokyah@gmail.com"
#'output:
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'  revealjs::revealjs_presentation:
#'    theme: white
#'    highlight: pygments
#'    center: true
#'    incremental: true
#'    transition: fade
#'    self_contained: false
#'    reveal_plugins: ["notes", "search"]
#'  md_document:
#'    variant: markdown_github
#'    toc: false
#'    toc_depth: 6
#'  pdf_document: default
#'title: "Generate the terrain and cover independent static variables for interpolation grid and for stations"
#'date: "20 June, 2018"
#'---

# inspired from https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r#140536
# https://atriplex.info/blog/index.php/2017/05/24/polygon-intersection-and-summary-with-sf/
# https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/intersect
# https://stackoverflow.com/questions/14682606/extract-value-from-raster-stack-from-spatialpolygondataframe
# https://stackoverflow.com/questions/42927384/r-handling-of-sf-objects-in-raster-package
# https://gis.stackexchange.com/questions/237133/function-sample-code-to-extract-raster-value-per-polygon-in-r

library(lwgeom)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(dplyr)
library(raster)
library(purrr)
library(rgeos)
library(reshape2)

# Avoid interference with old variables by cleaning the Global Environment
rm(list=ls(all=TRUE))

load("./data/data_source.SRTM.terrain.wallonia.90m.ras.rda")
load("./data/data_source.CLC.wallonia.polygons_sf.rda")
source("./R/geo.R")
source("./R/agromet_API.R")

#####
# FUNCTIONS DEFINITIONS
#####

cover_2mlr.fun <- function(data.sf) {

  # Delete geometry column
  data.df <- data.frame(data.sf)

  # Reshape data with CLASS labels as columns names
  # https://stackoverflow.com/questions/39053451/using-spread-with-duplicate-identifiers-for-rows
  data.df <- data.df %>%
    dplyr::select(sid, CLASS, cover_rate) %>%
    reshape2::dcast(sid ~ CLASS, fun = sum)

  # https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
  #data.df <- data.df[,c(1,2,5,4,3,6)]

  return(data.df)
}

#####
# DATA SOURCES
##

# renaming the datasource objects for code clarity
terrain.90.ras <- data_source.SRTM.terrain.wallonia.90m.ras
cover.sf <- data_source.clc_wallonia.polygons_sf
devtools::use_data(terrain.90.ras, overwrite = TRUE)
devtools::use_data(cover.sf, overwrite = TRUE)

#####
# EMPTY GRID + STATIONS
##

# Create the physical stations points location
stations.sf <- build.ps.locations.points_sf.fun(sf.bool = TRUE, EPSG.chr="3812")
stations.sf <- stations.sf %>% dplyr::select(one_of(c("sid", "geometry")))
devtools::use_data(stations.sf, overwrite = TRUE)

# Create the virtual stations interpolation grid points
grid.1000.pt.sf <- build.vs.grid.fun(
  res.num = 1000,
  geom.chr = "centers",
  sf.bool = TRUE,
  EPSG.chr = "3812",
  country_code.chr = "BE",
  NAME_1.chr = "Wallonie"
)
devtools::use_data(grid.1000.pt.sf, overwrite = TRUE)

# Create the virtual stations interpolation grid polygons
grid.1000.pg.sf <- build.vs.grid.fun(
  res.num = 1000,
  geom.chr = "polygons",
  sf.bool = TRUE,
  EPSG.chr = "3812",
  country_code.chr = "BE",
  NAME_1.chr = "Wallonie"
)
devtools::use_data(grid.1000.pg.sf, overwrite = TRUE)

#####
# CORINE LAND COVER
##

##
# COVER.GRID
##

# reproject the cover in the same CRS as grid and physical stations
sf::st_transform(cover.sf, sf::st_crs(grid.1000.pg.sf))
devtools::use_data(cover.sf, overwrite = TRUE)

# Make a 500m buffer around grid points
# https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
# https://stackoverflow.com/questions/46704878/circle-around-a-geographic-point-with-st-buffer
buffered.grid.sf <- sf::st_buffer(x = grid.1000.pt.sf, dist = 500)
devtools::use_data(buffered.grid.sf, overwrite = TRUE)

# extract cover information into the buffered grid point
cover.grid.sf <- sf::st_intersection(cover.sf, buffered.grid.sf)
cover.grid.sf <- cover.grid.sf %>%
  dplyr::mutate(
    customID = paste0(
      "buffer_",seq_along(1:nrow(cover.grid.sf))))
devtools::use_data(cover.grid.sf, overwrite = TRUE)

# create new column with area of each cover polygon for each buffered grid point
cover.summary.grid.sf <- cover.grid.sf %>%
  dplyr:: group_by(customID) %>%
  dplyr::summarise() %>%
  mutate(common_area = st_area(.))
devtools::use_data(cover.summary.grid.sf, overwrite = TRUE)

# Make a column with percentage of occupation of each land cover inside each grid point buffer
cover_rate.grid.sf <- sf::st_join(
  x = cover.grid.sf,
  y = cover.summary.grid.sf,
  join = st_covered_by
) %>%
  dplyr::select(sid, CLASS, common_area) %>%
  dplyr::mutate(cover_rate = as.numeric(common_area/(pi*100^2) * 100))
devtools::use_data(cover_rate.grid.sf, overwrite = TRUE)

# transposing to dataframe for data spreading (impossible (?) to achieve with dplyr spread)
cover_rate.grid.df <- cover_2mlr.fun(cover_rate.grid.sf)
colnames(cover_rate.grid.df) <- gsub(" ","_",colnames(cover_rate.grid.df))

# merge cover data with grid.1000.pt.sf
cover_rate.grid.sf = merge(grid.1000.pt.sf, cover_rate.grid.df, by = "sid")
devtools::use_data(cover_rate.grid.sf, overwrite = TRUE)

# only keep relevant columns
cover_rate.grid.sf <- cover_rate.grid.sf %>%
  dplyr::select(1,15:19)
devtools::use_data(cover_rate.grid.sf, overwrite = TRUE)

##########

# # extracting the cover information for each polygon of the grid
# cover.grid.sf <- sf::st_intersection(
#   lwgeom::st_make_valid(
#     cover.sf),
#   grid.1000.pg.sf
# )
# devtools::use_data(cover.grid.sf, overwrite = TRUE)
#
# # create a new customID column and keep only useful columns with dplyr select
# cover.grid.sf <- cover.grid.sf %>%
#   dplyr::mutate(
#     customID = paste0("poly_",
#                     seq_along(1:nrow(cover.grid.sf))
#     )
#   ) %>%
#   dplyr::select(ID, Area_ha, Shape_Leng, Shape_Area, CLASS, sid, customID, cell_area, geometry)
# devtools::use_data(cover.grid.sf, overwrite = TRUE)
#
# # create new column with area of each cover polygon for every cell of the grid
# cover.area.grid.sf <- cover.grid.sf %>%
#   dplyr::group_by(customID) %>%
#   dplyr::summarise() %>%
#   dplyr::mutate(common_area = sf::st_area(.))
# devtools::use_data(cover.area.grid.sf, overwrite = TRUE)
#
# # create new column with % area of each cover polygon for every cell of the grid
# cover_rate.grid.sf <- sf::st_join(
#   x = cover.grid.sf,
#   y = cover.area.grid.sf,
#   join = sf::st_covered_by) %>%
#   dplyr::mutate(cover_rate = as.numeric(common_area/cell_area * 100)) %>%
#   dplyr::select(sid, CLASS, common_area, cover_rate)
# devtools::use_data(cover_rate.grid.sf, overwrite = TRUE)
#
# cover_rate.grid.df <- cover_2mlr.fun(cover_rate.grid.sf)
# colnames(cover_rate.grid.df) <- gsub(" ","_",colnames(cover_rate.grid.df))
#
# # as the terrain grid was built using the grid points instead of polygons,
# # we need to intersect the grid polygons by points to have the same nrow for further joining
#
#
# a.cover_rate.grid.sf <- st_intersection(grid.1000.pt.sf, cover_rate.grid.sf)
# a.cover_rate.grid.sf  <- a.cover_rate.grid.sf  %>%
#   dplyr::select(one_of(c("sid", )))
#
# # merge cover data with grid.1000.pg.sf
# cover_rate.grid.sf = merge(grid.1000.pg.sf, cover_rate.grid.df, by = "sid")
# devtools::use_data(cover_rate.grid.sf, overwrite = TRUE)


##
# COVER.STATIONS
##

# Make a 100m buffer around stations
# https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
# https://stackoverflow.com/questions/46704878/circle-around-a-geographic-point-with-st-buffer
buffered.stations.sf <- sf::st_buffer(x = stations.sf, dist = 100)
devtools::use_data(buffer.stations.sf, overwrite = TRUE)

# extract cover information into the buffered stations
cover.stations.sf <- sf::st_intersection(cover.sf, buffered.stations.sf)
cover.stations.sf <-cover.stations.sf %>%
  dplyr::mutate(
    customID = paste0(
      "buffer_",seq_along(1:nrow(cover.stations.sf))))
devtools::use_data(cover.stations.sf, overwrite = TRUE)

# create new column with area of each cover polygon for each buffered station
cover.summary.stations.sf <- cover.stations.sf %>%
  dplyr:: group_by(customID) %>%
  dplyr::summarise() %>%
  mutate(common_area = st_area(.))
devtools::use_data(cover.summary.stations.sf, overwrite = TRUE)

# Make a column with percentage of occupation of each land cover inside the station buffer
cover_rate.stations.sf <- sf::st_join(
  x = cover.stations.sf,
  y = cover.summary.stations.sf,
  join = st_covered_by
  ) %>%
  dplyr::select(sid, CLASS, common_area) %>%
  dplyr::mutate(cover_rate = as.numeric(common_area/(pi*100^2) * 100))
devtools::use_data(cover_rate.stations.sf, overwrite = TRUE)

# transposing to dataframe for data spreading (impossible (?) to achieve with dplyr spread)
cover_rate.stations.df <- cover_2mlr.fun(cover_rate.stations.sf)
colnames(cover_rate.stations.df) <- gsub(" ","_",colnames(cover_rate.stations.df))

# merge cover data with stations.sf
cover_rate.stations.sf = merge(stations.sf, cover_rate.stations.df, by = "sid")
devtools::use_data(cover_rate.stations.sf, overwrite = TRUE)


#####
# TERRAIN
##

# reproject the terrain raster in the same CRS as grid and physical stations
terrain.90.ras <- raster::projectRaster(
  terrain.90.ras,
  crs = (sf::st_crs(grid.1000.pt.sf ))$proj4string
)
devtools::use_data(terrain.90.ras, overwrite = TRUE)

# intersecting  rasters with sf is not yet possible so extracting with sp.
grid.1000.pt.sp <- as(grid.1000.pt.sf,"Spatial")
stations.sp <- as(stations.sf,"Spatial")

##
# TERRAIN.GRID
##

# extracting terrain attributes with a 500m buffer at the grid and 100m at stations
# ::todo:: https://gis.stackexchange.com/questions/237133/function-sample-code-to-extract-raster-value-per-polygon-in-r
terrain.grid.sp <- extract(
  terrain.90.ras,
  grid.1000.pt.sp,
  buffer = 500,
  fun=mean,
  na.rm = TRUE,
  sp=TRUE
)

terrain.grid.sf <- sf::st_as_sf(terrain.grid.sp)
terrain.grid.sf <- terrain.grid.sf %>% dplyr::select(one_of(c("sid", "layer", "slope", "aspect", "roughness", "geometry")))
devtools::use_data(terrain.grid.sf, overwrite = TRUE)

##
# TERRAIN.STATIONS
##

terrain.stations.sp <- extract(
  terrain.90.ras,
  stations.sp,
  buffer = 100,
  fun=mean,
  na.rm = TRUE,
  sp=TRUE
)

terrain.stations.sf <- sf::st_as_sf(terrain.stations.sp)
devtools::use_data(terrain.stations.sf, overwrite = TRUE)

#####
# TERRAIN + COVER as DATAFRAME
##

expl.static.grid.sf <- terrain.grid.sf %>% sf::st_join(cover_rate.grid.sf)
expl.static.grid.sf <- expl.static.grid.sf %>% dplyr::select(
  one_of(c("sid.x", "layer", "slope", "aspect", "roughness", "Agricultural_areas", "Artificials_surfaces", "Forest", "Herbaceous_vegetation", "geometry")))
colnames(expl.static.grid.sf) <- c("gid", "altitude", "slope", "aspect", "roughness",
                                   "crops", "artificial", "forest", "herbaceous", "geometry")
devtools::use_data(expl.static.grid.sf, overwrite = TRUE)

expl.static.grid.df <- dplyr::bind_cols(expl.static.grid.sf, data.frame(sf::st_coordinates(expl.static.grid.sf)))
devtools::use_data(expl.static.grid.df, overwrite = TRUE)

#+ ---------------------------------
#' ## Terms of service
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - hello.pokyah@gmail.com 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
