#'---
#'author: "Thomas Goossens - pokyah.github.io"
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
#'title: "highRes terrain raster stack for Wallonia"
#'date: "15 June, 2018"
#'---

source("./R/geo.R")
data_source.SRTM.terrain.wallonia.90m.ras <- build.SRTM.terrain.90m.ras.fun(
  country_code.chr = "BE",
  NAME_1.chr="Wallonie",
  aggregation_factor.num=NULL,
  EPSG.chr="3812",
  path.chr = "./external-data")


devtools::use_data(data_source.SRTM.terrain.wallonia.90m.ras, overwrite = TRUE)
