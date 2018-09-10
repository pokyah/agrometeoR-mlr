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
#'title: "Get a polygons sf of Corine Land Cover for Wallonia with the predefined classses custom reclassification"
#'date: "15 June, 2018"
#'---

# Lambert 2008
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Load spatial data from Wallonia limits
be.sp <- raster::getData('GADM', country = 'BE', level = 1, download = TRUE)
be.sp$NAME_1
wallonie.sp <- be.sp[be.sp$NAME_1 == "Wallonie",]
wallonie.3812.poly.sp <- sp::spTransform(wallonie.sp, sp::CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))

# Download CORINE land cover for Belgium from http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-en.xml
corine.sp <- maptools::readShapePoly("./external-data/Corine_Land_Cover/CLC12_BE.shp")

# We know the crs from the metadata provided on the website http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-en.xml
raster::crs(corine.sp) <- lambert2008.crs

# CORINE land cover for Wallonia
corine.wal.sp <- raster::crop(corine.sp, wallonie.3812.poly.sp)

# Download legend for CLC
download.file("http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1/corine-land-cover-classes-and/clc_legend.csv/at_download/file",
              destfile = "./external-data/Corine_Land_Cover/clc_legend.csv")
legend <- read.csv(file = "./external-data/Corine_Land_Cover/clc_legend.csv", header = TRUE, sep = ",")

# Legend codes present in Wallonia
legend.code.wal <- base::data.frame(base::unique(corine.wal.sp$code_12))

# Legend for CORINE land cover in Wallonia
# https://stackoverflow.com/questions/38850629/subset-a-column-in-data-frame-based-on-another-data-frame-list
legend.wal <- base::subset(legend, CLC_CODE %in% legend.code.wal$unique.corine.wal.sp.code_12.)

# CLC_CODE class from integer to numeric
legend.wal$CLC_CODE <- base::as.numeric(legend.wal$CLC_CODE)

corine.wal.sf <- sf::st_as_sf(corine.wal.sp)
corine.wal.sf$code_12 <- base::as.numeric(base::paste(corine.wal.sf$code_12))

# Reclass all types of CLC to create 6 groups
data_source.clc.wallonia.polygons_sf <-
  sf::st_as_sf(
    dplyr::mutate(
      corine.wal.sf,
      CLASS = dplyr::case_when(
        code_12 <= 142 ~ "Artificials surfaces",
        code_12 == 211 ~ "Agricultural areas",
        code_12 == 222 ~ "Agricultural areas",
        code_12 == 231 ~ "Herbaceous vegetation",
        code_12 == 242 ~ "Agricultural areas",
        code_12 == 243 ~ "Agricultural areas",
        code_12 == 311 ~ "Forest",
        code_12 == 312 ~ "Forest",
        code_12 == 313 ~ "Forest",
        code_12 == 321 ~ "Herbaceous vegetation",
        code_12 == 322 ~ "Herbaceous vegetation",
        code_12 == 324 ~ "Forest",
        code_12 > 400 ~ "Water"))
  )

devtools::use_data(data_source.clc.wallonia.polygons_sf, overwrite = TRUE)
