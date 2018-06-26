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
#'title: "Extract terrain raster data at virtual station locations"
#'date: "20 June, 2018"
#'---

# inspired from https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r#140536
# https://atriplex.info/blog/index.php/2017/05/24/polygon-intersection-and-summary-with-sf/
# https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/intersect
# https://stackoverflow.com/questions/14682606/extract-value-from-raster-stack-from-spatialpolygondataframe
# https://stackoverflow.com/questions/42927384/r-handling-of-sf-objects-in-raster-package

library(lwgeom)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(dplyr)
library(raster)
library(purrr)
library(rgeos)

load("./data/vs.grid.1km.wallonia.polygons_sf.rda")
load("./data/ps.locations.wallonia.polygons_sf.rda")
load("./data/data_source.SRTM.terrain.wallonia.90m.ras.rda")




# reproject the raster in the same CRS as vs grid
data_source.SRTM_terrain.wallonia.90m.ras <- raster::projectRaster(
  data_source.SRTM.terrain.wallonia.90m.ras,
  crs = (sf::st_crs(vs.grid.1km.wallonia.polygons_sf))$proj4string
  )

# intersecting raster with sf is not yet possible so extracting with sp. The result is a list !
vs.grid.1km.wallonia.polygons_sp <- as(vs.grid.1km.wallonia.polygons_sf,"Spatial")

# https://gis.stackexchange.com/questions/237133/function-sample-code-to-extract-raster-value-per-polygon-in-r
raster.extracted.df <- extract(
  data_source.SRTM.terrain.wallonia.90m.ras,
  vs.grid.1km.wallonia.polygons_sp,
  fun=mean,
  na.rm = TRUE,
  df=TRUE
)

vs.grid.1km.wallonia.polygons_sp@data <- raster.extracted.df
vs.grid.1km.intersect.SRTM.terrain.wallonia.90m.polygon_sf <- sf::st_as_sf(vs.grid.1km.wallonia.polygons_sp)
devtools::use_data(vs.grid.1km.intersect.SRTM.terrain.wallonia.90m.polygon_sf)

#
#
#
#
# # reclassifying the raster
# # https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/reclassify
# # http://r-sig-geo.2731867.n2.nabble.com/Re-Reclassification-of-raster-td7591719.html
#
# reclassify.raster <- function(raster, quantileSeq.num){
#   # classify the pixels according to the quantile each pixel belongs to
#   ix <- findInterval(
#     raster::getValues(
#       raster),
#     (raster::quantile(raster, quantileSeq.num))
#   )
#
#   # make a classified version of rASTER (class 0, 1 or 3)
#   classified_r <- raster::setValues(
#     raster,
#     ix)
# }
#
# # reclassified <- lapply(data_source.SRTM_terrain.wallonia.90m.ras, reclassify.raster, seq(from = 0, to = 1, by = 0.1))
# reclassified.ras <- raster::stack(lapply(
#   (raster::stack(data_source.SRTM_terrain.wallonia.90m.ras))@layers,
#   reclassify.raster,
#   seq(from = 0, to = 1, by = 0.1)))
#
# ## Some slope and orientation calculations have returned NA values (border). Impute them a value for next part to work
# # https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel
# # https://cran.r-project.org/web/packages/gapfill/gapfill.pdf
#
#
#
#
# # https://gis.stackexchange.com/questions/102870/step-by-step-how-do-i-extract-raster-values-from-polygon-overlay-with-q-gis-or
# # Get class counts for each polygon
# reclassified_r_extract.l <- raster::extract(
#   reclassified.ras,
#   vs.grid.1km.wallonia.polygons_sp
# )
#
# #converting each cell from matrix to dataframe
# reclassified_r_extract.l <- reclassified_r_extract.l %>% purrr::map( function(x) as.data.frame(x))
#
# # function to calculate the mean of aspect, slope, elevation of each cell
# compute.terrain_means <- function(cell.df){
#
#   cell.df <- as.data.frame(
#     cell.df %>%
#       purrr::map(
#         function(x)mean(x, na.rm = TRUE)))
# }
#
# means.reclassified_r_extract.l <- reclassified_r_extract.l %>% purrr::map(compute.terrain_means)
#
#
# # https://gis.stackexchange.com/questions/237133/function-sample-code-to-extract-raster-value-per-polygon-in-r
#
#
# # function to calculate the % of each class for each cell
# compute.terrain_cat_percentages <- function(cell.df){
#   # categorizing data according to quantiles of whole cell
#   # belongs_to <- cell.df %>%
#   #   dplyr::mutate(elevation_group = cut(layer, breaks= terrain.quantiles[[1]], na.rm=TRUE, labels = FALSE)) %>%
#   #   dplyr::mutate(slope_group = cut(slope, breaks= terrain.quantiles[[2]], na.rm=TRUE, labels = FALSE)) %>%
#   #   dplyr::mutate(aspect_group = cut(aspect, breaks= terrain.quantiles[[3]], na.rm=TRUE, labels = FALSE)) %>%
#   #   dplyr::mutate(roughness_group = cut(roughness, breaks= terrain.quantiles[[4]], na.rm=TRUE, labels = FALSE)) %>%
#   #   dplyr::select(contains("_group"))
#
#   #imputing NA to mean of current cell
#   cell.df <- as.data.frame(
#     cell.df %>%
#       purrr::map(
#         function(x) replace(x, is.na(x), ceiling(mean(as.numeric(x), na.rm = TRUE)))),
#     type=factor(as.character(1:(length(seq(from = 0, to = 1, by = 0.1)))),as.character(1:(length(seq(from = 0, to = 1, by = 0.1)))))
#   )
#
#   # Converting to factor with number of levels = number of classes defined
#   cell.df[] <- lapply(cell.df, factor, as.character(1:(length(seq(from = 0, to = 1, by = 0.1)))))
#
#   # computing the percentages of each level
#   percentages <- as.data.frame(cell.df %>%
#                                  purrr::map(
#                                    function(x) summary(x)/sum((summary(x)))*100)
#   )
#
# }
#
# # calculating the % of each class for each cell for each layer
#
# # testing on a subset
# # test.l <-
# percentages.l <- reclassified_r_extract.l[1:3] %>% purrr::map(compute.terrain_cat_percentages)
#
# data.frame(cat = row.names(percentages.l[[1]]))
# test.cat.df <- dplyr::bind_cols(percentages.l[[1]],data.frame(cat = row.names(percentages.l[[1]])) )
#
# terrain.params <- list(
#   elevation = trans.test.cat.f[1,],
#   slope = trans.test.cat.f[2,],
#   aspect = trans.test.cat.f[3,],
#   roughness = trans.test.cat.f[4,]
# )
#
# terrain.vars <- do.call("cbind", terrain.params)
#
# # Get class counts for each polygon
# # compute.percentages <- function(dataframe){
# #   dataframe %>%
# #     purrr::map( function(x) table(x) )
# # }
# #
# #
# # v.counts <- lapply(reclassified_r_extract.l,compute.percentages)
#
#
#
#
#
#
#
# # now we need to compute our indicators for each polygon of the list before appending these to the original vs grid sf
# # max and min values of each raster of the stack
#
# # terrain.maxValues <- raster::maxValue(data_source.SRTM_terrain.wallonia.90m.ras)
# # terrain.minValues <- raster::minValue(data_source.SRTM_terrain.wallonia.90m.ras)
# # terrain.quantiles <- raster::quantile(data_source.SRTM_terrain.wallonia.90m.ras)
# # terrain.quantiles <- data.frame(t(terrain.quantiles))
#
#
# #http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
#
#
# #vs.grid.1km.intersect.SRTM_terrain.wallonia.90m.l <- lapply(vs.grid.1km.intersect.SRTM_terrain.wallonia.90m.l, as.data.frame )
#
#
#
#
#
#
#
#
# # making the sp back a sf
# vs.grid.1km.intersect.SRTM_terrain.wallonia.90m.polygons_sf <- sf::st_as_sf(
#   vs.grid.1km.intersect.SRTM_terrain.wallonia.90m.polygons_sp)
#
# # converting the raster to sp object
# data_source.SRTM_terrain.wallonia.90m.polygon_sp <- rasterToPolygons(
#   data_source.SRTM_terrain.wallonia.90m.ras, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
#
#
#
# # making the intersection of the vs grid sf and the terrain sf
# vs.grid.1km.intersect.clc.wallonia.polygons_sf <- sf::st_intersection(
#   lwgeom::st_make_valid(
#     data_source.SRTM_terrain.wallonia.90m.polygon_sf),
#   vs.grid.1km.wallonia.polygons_sf
# )
#
# # vs.grid.1km.intersect.SRTM.terrain.wallonia.90m.elevation.polygon_sp <- raster::intersect(
# #   vs.grid.1km.wallonia.polygons_sp,
# #   data_source.SRTM_terrain.wallonia.90m.ras,
# # )
#
#
# # create a new column with custom id and select columns
# vs.grid.1km.intersect.clc.wallonia.polygons_sf <- dplyr::mutate(vs.grid.1km.intersect.clc.wallonia.polygons_sf,
#                                                       customID = base::paste0("poly_",
#                                                                               seq_along(1:nrow(
#                                                                                 vs.grid.1km.intersect.clc.wallonia.polygons_sf
#                                                                               )))) %>%
#   dplyr::select(ID, Area_ha, Shape_Leng, Shape_Area, CLASS, sid, customID, area, geometry)
#
# # create new column with area of each polygon for every cell
# vs.grid.1km.polygon_area_per_cell.clc.wallonia.polygons_sf <- dplyr::group_by(vs.grid.1km.intersect.clc.wallonia.polygons_sf, customID) %>%
#   dplyr::summarise() %>%
#   dplyr::mutate(common_area = sf::st_area(.))
#
# # create a column with percentage of cover of every CLC in each cell
# vs.grid.1km.intersect.clc.wallonia.polygons_sf <- sf::st_join(x = vs.grid.1km.intersect.clc.wallonia.polygons_sf, y = vs.grid.1km.polygon_area_per_cell.clc.wallonia.polygons_sf, join = sf::st_covered_by) %>%
#   dplyr::mutate(rate_cover = base::as.numeric(common_area/AREA * 100)) %>%
#   dplyr::select(sid, CLASS, common_area, rate_cover)
#
#
# devtools::use_data(vs.grid.1km.intersect.clc.wallonia.polygons_sf, overwrite = TRUE)
# # rgdal::writeOGR(as(wal_grid_1km.sf, "Spatial"), dsn = './', layer = 'wal_poly_grid', driver = "ESRI Shapefile")

#+ ---------------------------------
#' ## Terms of service
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - hello.pokyah@gmail.com 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
