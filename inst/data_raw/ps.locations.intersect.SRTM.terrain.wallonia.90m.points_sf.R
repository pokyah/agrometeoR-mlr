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
#'title: "Extract raster data at the location of physical stations with a buffer"
#'date: "20 June, 2018"
#'---

# inspired from http://mbjoseph.github.io/2014/11/08/nlcd.html

load("./data/ps.locations.wallonia.points_sf.rda")
load("./data/data_source.SRTM.terrain.wallonia.90m.ras.rda")

# reproject the raster in the same CRS as ps points
data_source.SRTM_terrain.wallonia.90m.ras <- raster::projectRaster(
  data_source.SRTM_terrain.wallonia.90m.ras,
  crs = (sf::st_crs(ps.locations.wallonia.points_sf))$proj4string
  )

# intersecting raster with sf is not yet possible so extracting with sp. The result is a list !
ps.locations.wallonia.points_sp <- as(vs.grid.1km.wallonia.points_sf,"Spatial")

# https://gis.stackexchange.com/questions/237133/function-sample-code-to-extract-raster-value-per-polygon-in-r
raster.extracted.df <- extract(
  data_source.SRTM_terrain.wallonia.90m.ras,
  ps.locations.wallonia.points_sp,
  buffer = 200,
  fun=mean,
  na.rm = TRUE,
  df=TRUE
)


ps.locations.wallonia.points_sp@data <- raster.extracted.df
ps.locations.intersect.SRTM.terrain.wallonia.90m.points_sf <- sf::st_as_sf(ps.locations.wallonia.points_sp)
devtools::use_data(ps.locations.intersect.SRTM.terrain.wallonia.90m.points_sf)


#+ ---------------------------------
#' ## Terms of service
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - hello.pokyah@gmail.com 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
