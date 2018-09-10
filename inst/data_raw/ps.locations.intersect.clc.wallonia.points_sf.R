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
#'title: "Extract terrain data at location of physical stations with a buffer"
#'date: "25 June, 2018"
#'---

load("./data/ps.locations.wallonia.points_sf.rda")
load("./data/data_source.clc.wallonia.polygons_sf.rda")
source("./R/geo.R")

ps.locations.intersect_buffer.clc.wallonia.polygons_sf <- append_clc_classes_buffer(corine.wal.simple.sf = data_source.clc_wallonia.polygons_sf, radius.num = 500, locations.sf = ps.locations.wallonia.points_sf)
devtools::use_data(ps.locations.intersect_buffer.clc.wallonia.polygons_sf, overwrite = TRUE)

#+ ---------------------------------
#' ## Terms of service
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
