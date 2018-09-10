#'---
#'author: "Thomas Goossens (CRA-W) - t.goossens@cra.wallonie.be"
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
#'title: "Collection of R Scripts of the Agromet project"
#'date: "25 June, 2018"
#'---

load("./data/vs.grid.1km.wallonia.polygons_sf.rda")
load("./data/data_source.CLC.wallonia.polygons_sf.rda")


vs.grid.1km.intersect.clc.wallonia.polygons_sf <- sf::st_intersection(
  lwgeom::st_make_valid(
    data_source.clc_wallonia.polygons_sf),
  vs.grid.1km.wallonia.polygons_sf
  )

# create a new column with custom id and select columns
vs.grid.1km.intersect.clc.wallonia.polygons_sf <- dplyr::mutate(vs.grid.1km.intersect.clc.wallonia.polygons_sf,
                                                      customID = base::paste0("poly_",
                                                                              seq_along(1:nrow(
                                                                                vs.grid.1km.intersect.clc.wallonia.polygons_sf
                                                                              )))) %>%
  dplyr::select(ID, Area_ha, Shape_Leng, Shape_Area, CLASS, sid, customID, area, geometry)

# create new column with area of each polygon for every cell
vs.grid.1km.polygon_area_per_cell.clc.wallonia.polygons_sf <- dplyr::group_by(vs.grid.1km.intersect.clc.wallonia.polygons_sf, customID) %>%
  dplyr::summarise() %>%
  dplyr::mutate(common_area = sf::st_area(.))

# create a column with percentage of cover of every CLC in each cell
vs.grid.1km.intersect.clc.wallonia.polygons_sf <- sf::st_join(x = vs.grid.1km.intersect.clc.wallonia.polygons_sf, y = vs.grid.1km.polygon_area_per_cell.clc.wallonia.polygons_sf, join = sf::st_covered_by) %>%
  dplyr::mutate(rate_cover = base::as.numeric(common_area/AREA * 100)) %>%
  dplyr::select(sid, CLASS, common_area, rate_cover)


devtools::use_data(vs.grid.1km.intersect.clc.wallonia.polygons_sf, overwrite = TRUE)
# rgdal::writeOGR(as(wal_grid_1km.sf, "Spatial"), dsn = './', layer = 'wal_poly_grid', driver = "ESRI Shapefile")

#+ ---------------------------------
#' ## Terms of service
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
