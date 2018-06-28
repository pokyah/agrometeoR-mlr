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

install.packages("lwgeom")
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

# load("./data/data_source.SRTM.terrain.wallonia.90m.ras.rda")
# load("./data/data_source.CLC.wallonia.polygons_sf.rda")
source("./R/geo.R")
source("./R/agromet_API.R")

#####
# DATA SOURCES
##

##
# TERRAIN
##
terrain.90.ras <- build.SRTM.terrain.90m.ras.fun(
  country_code.chr = "BE",
  NAME_1.chr="Wallonie",
  aggregation_factor.num=NULL,
  EPSG.chr="3812",
  path.chr = "./external-data")
devtools::use_data(terrain.90.ras, overwrite = TRUE)

##
# COVER
##
cover.sf <- build_cover.sf.fun(
  country_code.chr= "BE",
  NAME_1.chr = "Wallonie",
  EPSG.chr = "3812",
  path.corine.shapefile.chr = "./external-data/Corine_Land_Cover/CLC12_BE.shp",
  EPSG.corine.chr = "3812")
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


#####
# COVER
##

##
# COVER.GRID
##
cover_rate.grid.sf <- get.points.cover_pct.fun(
  cover.sf = cover.sf ,
  points.sf = grid.1000.pt.sf,
  radius.num = 500)

##
# COVER.STATIONS
##
cover_rate.stations.sf <- get.points.cover_pct.fun(
  cover.sf = cover.sf ,
  points.sf = stations.sf,
  radius.num = 100)

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

##
#GRID
##

expl.static.grid.sf <- terrain.grid.sf %>% sf::st_join(cover_rate.grid.sf)
expl.static.grid.sf <- expl.static.grid.sf %>% dplyr::select(
  one_of(c("sid.x", "layer", "slope", "aspect", "roughness", "Agricultural_areas", "Artificials_surfaces", "Forest", "Herbaceous_vegetation", "Water", "geometry")))
colnames(expl.static.grid.sf) <- c("gid", "altitude", "slope", "aspect", "roughness","crops", "artificial", "forest", "herbaceous", "water", "geometry")
devtools::use_data(expl.static.grid.sf, overwrite = TRUE)

expl.static.grid.df <- dplyr::bind_cols(expl.static.grid.sf, data.frame(sf::st_coordinates(expl.static.grid.sf)))
devtools::use_data(expl.static.grid.df, overwrite = TRUE)

##
#STATIONS
##
expl.static.stations.sf <- terrain.stations.sf %>% sf::st_join(cover_rate.stations.sf)
expl.static.stations.sf <- expl.static.stations.sf %>% dplyr::select(
  one_of(c("sid.x", "layer", "slope", "aspect", "roughness", "Agricultural_areas", "Artificials_surfaces", "Forest", "Herbaceous_vegetation", "Water", "geometry")))
colnames(expl.static.stations.sf) <- c("gid", "altitude", "slope", "aspect", "roughness","crops", "artificial", "forest", "herbaceous", "geometry")
devtools::use_data(expl.static.stations.sf, overwrite = TRUE)

expl.static.stations.df <- dplyr::bind_cols(expl.static.stations.sf, data.frame(sf::st_coordinates(expl.static.stations.sf)))
devtools::use_data(expl.static.stations.df, overwrite = TRUE)

#+ ---------------------------------
#' ## Terms of service
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - hello.pokyah@gmail.com 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
