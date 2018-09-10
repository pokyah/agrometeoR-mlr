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
#'title: "Getting the temperature and solar irradiation data from the AGROMET API"
#'date: "20 June, 2018"
#'---

# Read data from the API exported json file downloaded from PAMESEB FTP & create a datframe
records.l <- jsonlite::fromJSON(
    "./inst/data_raw/data_files/cleandataSensorstsa-ensForallFm2015-11-11To2018-06-30.json") # available on FTP
records.df <- records.l$results
stations_meta.df <- records.l$references$stations
records_and_stations_meta.l <- list(stations_meta.df = stations_meta.df, records.df = records.df)
records.stations.df <- agrometAPI::prepare_agromet_API_data.fun(records_and_stations_meta.l, "cleandata")

# Filtering records to keep only the useful ones (removing non relevant stations)
records.stations.df <- records.stations.df %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(!is.na(to)) %>%
  filter(state == "Ok") %>%
  filter(!is.na(tsa)) %>%
  filter(!is.na(ens))

# Selecting only the useful features
records.stations.df <- records.stations.df %>%
  dplyr::select("mtime", "sid", "ens", "longitude", "latitude", "tsa")

# Renaming sid to gid
colnames(records.stations.df)[2] <- "gid"

# Create & save the the .rda object in the data folder of the package
devtools::use_data(records.stations.df, overwrite = TRUE)

#+ ---------------------------------
#' ## Terms of service
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - hello.pokyah@gmail.com 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
