#'---
#'author: "Loïc Davadan - ldavadan.github.io & Thomas Goossens - pokyah.github.io"
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
#'title: "Create the Pameseb physical stations sf object with static independant variables"
#'date: "15 June, 2018"
#'---

library(lwgeom)
ps.locations.wallonia.points_sf <- build.ps.locations.points_sf.fun(sf.bool = TRUE, EPSG.chr="3812")
devtools::use_data(ps.locations.wallonia.points_sf, overwrite = TRUE)
