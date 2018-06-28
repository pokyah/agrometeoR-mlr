#' Build a topographic rasters stack (elevation, slope, aspect)
#'
#' This function builds a topographic rasters stack for Belgium (elevation, slope, aspect). The rasters are projected in the
#' 3812 EPSG code. No input parameters are required.
#' @author Thomas Goossens - pokyah.github.io
#' @return A stack of topographic rasters
#' @export
build_lowRes_terrain_rasters.fun <- function() {
  # Get the Belgium DEM
  bel.ele.ras = raster::getData("alt", country = "BE", mask = TRUE)

  # The data are not projected but are in longlat so we need to project it to get the distance units
  bel.ele.ras <- raster::projectRaster(bel.ele.ras, crs = toString((dplyr::filter(rgdal::make_EPSG(), code=="3812"))$prj4))

  # compute the slope from the elevation
  bel.slope.ras <- raster::terrain(bel.ele.ras, opt="slope", unit="degrees")

  # compute the aspect from the elevation
  bel.aspect.ras <- raster::terrain(bel.ele.ras, opt="aspect", unit="degrees")

  # create the stack of rasters
  topo.stack.ras <- stack(bel.ele.ras, bel.slope.ras, bel.aspect.ras)

  # Return the stack of rasters
  return(topo.stack.ras)
}

#' Build a high resolution topographic rasters stack (elevation, slope, aspect)
#'
#' This function builds a topographic rasters stack for Belgium (elevation, slope, aspect). The rasters are projected in the
#' 3812 EPSG code.
#' @author Thomas Goossens - pokyah.github.io
#' @param country_code.chr a character specifying the ISO contrycode. Ex : BE for belgium
#' @param NAME_1.chr a character specifying the NAME_1 value for lower than country level information
#' @param aggregation_factor.num a numeric specifying the aggregation factor to get the desired spatial resolution
#' @param EPSG.chr a character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @param path.chr a character specifying the path where to dowload the SRTM data
#' @return A stack of topographic rasters
#' @export
build.SRTM.terrain.90m.ras.fun <- function(country_code.chr, NAME_1.chr=NULL, aggregation_factor.num=NULL, EPSG.chr=NULL, path.chr) {

  # Path to downloaded SRTM Tiles refs
  srtm.tiles.ref <- raster::shapefile("./external-data/Digital_Elevation_Model/90m_resolution/srtm/tiles.shp")

  # Get country geometry first
  if(length(list.files(paste0(path.chr,"/Boundaries"), all.files = TRUE, include.dirs = TRUE, no.. = TRUE))>0){
    extent.sp <- readRDS(paste0(path.chr,"/Boundaries/", "GADM_2.8_BEL_adm1.rds"))
  }else{
    extent.sp <- raster::getData('GADM', country=country_code.chr, level=1)
    crs <- crs(extant.sp)
  }
  if(!is.null(NAME_1.chr)){
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  }

  # to compute slope, aspect, etc, we need neighbourings pixels out of extent boundary .So we buffer it :
  # https://gis.stackexchange.com/questions/234135/enlarging-polygon-slightly-using-r
  extent.sf <- sf::st_transform(sf::st_as_sf(extent.sp), 3812)
  larger.extent.sp <- rgeos::gBuffer(as(extent.sf, "Spatial"), width = 5000)
  larger.extent.sp <- spTransform(larger.extent.sp, crs(extent.sp))

  # Intersect extent geometry and tile grid
  intersects <- rgeos::gIntersects(larger.extent.sp, srtm.tiles.ref, byid=T)
  tiles      <- srtm.tiles.ref[intersects[,1],]

  # Download tiles using getData

  # inspired from https://www.gis-blog.com/download-srtm-for-an-entire-country/
  srtm_list  <- list()

    # if(length(list.files(paste0(path.chr,"/Digital_Elevation_Model/90m_resolution/download"), all.files = TRUE, include.dirs = TRUE, no.. = TRUE))>0){
    #   tile_files <- list.files(path.chr, pattern = "srtm")
    #   unique(sort(list.files(path.chr, pattern = "tif")))
    #   ## unique tile names
    #   tile_names <- lapply(
    #     strsplit((unique(list.files(paste0(path.chr, "/Digital_Elevation_Model/90m_resolution/download"), pattern = "tif"))), split = "\\."),
    #     function(x) x[1]
    #   )
    #
    #   ## unique extensions
    #   extensions <- unique(sapply(
    #     strsplit((unique(list.files(paste0(path.chr, "/Digital_Elevation_Model/90m_resolution/download"), pattern = "srtm"))), split = "\\."),
    #     function(x) x[2]
    #   ))
    #
    #   srtm_list <- lapply(
    #     tile_names,
    #     function(x) tile <- raster::raster(paste0(path.chr, "/Digital_Elevation_Model/90m_resolution/download", "/", x,".tif"))
    #   )
    #  }else{
      for(i in 1:length(tiles)){
        lon <- raster::extent(tiles[i,])[1]  + (raster::extent(tiles[i,])[2] - raster::extent(tiles[i,])[1]) / 2
        lat <- raster::extent(tiles[i,])[3]  + (raster::extent(tiles[i,])[4] - raster::extent(tiles[i,])[3]) / 2


        tile <- raster::getData('SRTM', #data are downloaded from http://www.cgiar-csi.org/. See getData do of pokyah/raster repo on github
                                lon=lon,
                                lat=lat,
                                download = TRUE,
                                path = path.chr)
        srtm_list[[i]] <- tile
     }
   #}

  # Mosaic tiles
  srtm_list$fun <- mean
  devtools::use_data(srtm_list, overwrite = TRUE)
  srtm_mosaic.ras <- do.call(raster::mosaic, srtm_list)
  devtools::use_data(srtm_mosaic.ras, overwrite = TRUE)

  # Crop tiles to extent borders
  extent.elevation.ras <- raster::crop(srtm_mosaic.ras, larger.extent.sp)
  extent.elevation.ras <- raster::mask(extent.elevation.ras, larger.extent.sp)

  # transform to desired CRS
  if(!is.null(EPSG.chr)){
    raster::projectRaster(extent.elevation.ras, crs = toString((dplyr::filter(rgdal::make_EPSG(), code==EPSG.chr))$prj4))
  }

  # aggregate to lower resolution
  # inspired from https://stackoverflow.com/questions/32278825/how-to-change-the-resolution-of-a-raster-layer-in-r
  if(!is.null(aggregation_factor.num)){
    extent.elevation.ras <- raster::aggregate(extent.elevation.ras, fact=aggregation_factor.num)
  }

  # compute the slope from the elevation
  # inspired from https://rpubs.com/etiennebr/visualraster
  extent.slope.ras <- raster::terrain(extent.elevation.ras, opt="slope", unit="degrees")
  extent.aspect.ras <- raster::terrain(extent.elevation.ras, opt="aspect", unit="degrees")
  extent.roughness.ras <- raster::terrain(extent.elevation.ras, opt="roughness")

  # stack the rasters
  extent.terrain.ras = raster::stack(
    extent.elevation.ras,
    extent.slope.ras,
    extent.aspect.ras,
    extent.roughness.ras)

  # crop to non enlarged extent
  extent.terrain.ras <- raster::crop(extent.terrain.ras, extent.sp)
  devtools::use_data(extent.terrain.ras, overwrite = TRUE)
}

#' Build a sp/sf that contains the locations of the Pameseb automatic weather stations
#'
#' This function builds a spatial sp object that contains the he rasters are projected in the
#' 3812 EPSG code. No input parameters are required.
#' @author Thomas Goossens - pokyah.github.io
#' @param sf.bool A boolean specifying if we want as sp or sf (TRUE for sf)
#' @param EPSG.chr a character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @return A sp spatial grid with the desired resolution clipped to the Wallonia Polygon
#' @export
build.ps.locations.points_sf.fun <- function(sf.bool, EPSG.chr){
  # proj4 of the Agromet API data
  proj4.chr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # Retrieving useless data from API
  demo.records.df <- prepare_agromet_API_data.fun(
    get_from_agromet_API.fun(
      user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
      table_name.chr = "cleandata",
      stations_ids.chr = "all",
      sensors.chr = "tsa",
      dfrom.chr = as.character(Sys.Date()-60),
      dto.chr = as.character(Sys.Date()-59),
      api_v.chr = "v2"
    ), table_name.chr = "cleandata"
  )

  # Filtering records to keep only the useful ones (removing unecessary stations)
  demo.records.df <- dplyr::filter(demo.records.df, network_name == "pameseb")
  demo.records.df <- dplyr::filter(demo.records.df, type_name != "Sencrop")
  demo.records.df <- dplyr::filter(demo.records.df, !is.na(to))
  demo.records.df <- dplyr::filter(demo.records.df, state == "Ok")
  demo.records.df <- dplyr::filter(demo.records.df, !is.na(tsa))

  # Selecting only the useful features
  demo.records.df <- dplyr::select(demo.records.df, one_of(c("sid", "mtime", "longitude", "latitude", "altitude")))

  # defining the stations locations sp object
  stations.df <- dplyr::filter( demo.records.df, mtime == min(mtime, na.rm = TRUE))

  stations.sf <- sf::st_as_sf(
    x = stations.df,
    coords = c("longitude", "latitude"),
    crs = proj4.chr)

  # transform to desired CRS
  if(!is.null(EPSG.chr)){
    stations.sf <- sf::st_transform(x = stations.sf, crs = as.numeric(EPSG.chr) )
  }

  if(sf.bool == TRUE){
    stations.sf
  }
  # else transform to sf
  else
  {
    stations.sp <- as(stations.sf, "Spatial")
  }

}

#' Build a sp/sf interpolation grid with the desired spatial resolution for Wallonia
#'
#' Inspired from https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points,
#' https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf, https://gis.stackexchange.com/questions/22843/converting-decimal-degrees-units-to-km-in-r,
#' https://stackoverflow.com/questions/48727511/r-grid-of-points-from-polygon-input
#' @author Thomas Goossens - pokyah.github.io
#' @param country_code.chr a character specifying the ISO contrycode. Ex : BE for belgium
#' @param NAME_1.chr a character specifying the NAME_1 value for lower than country level information
#' @param res.num A numeric representing the spatial resolution of the desired grid (in meters)
#' @param geom.chr A character specifying the geometry of the interpolation grid. Cant take a value among ("polygons", "centers" or "corners")
#' @param sf.bool A boolean specifying if we want as sp or sf (TRUE for sf)
#' @param EPSG.chr A character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @return A sf spatial grid with the desired resolution clipped to the Wallonia Polygon
#' @export
build.vs.grid.fun <- function(country_code.chr, NAME_1.chr, res.num, geom.chr, sf.bool, EPSG.chr = NULL){ #build.SRTM_terrain.90m.ras.fun

  # Get country geometry first
  extent.sp <- raster::getData('GADM', country=country_code.chr, level=1, path =  "./external-data/Boundaries")

  if(!is.null(NAME_1.chr)){
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  }

  # convert from geographic lat/lon to projected EPSG for Belgian Lambert 2008 (needed to make grid in meters)
  extent.sf <- sf::st_transform(sf::st_as_sf(extent.sp), 3812)

  # make the grid and clip it with the extent boundaries
  grid.sf <- sf::st_intersection(
    sf::st_sf(
      sf::st_make_grid(
        extent.sf, cellsize= res.num, n=c(500,500), what= geom.chr, crs = 3812)
    ),
    extent.sf)

  # Create column cell_area of each cell
  if(geom.chr=="polygons"){
    grid.sf <- grid.sf %>% dplyr::mutate(cell_area = sf::st_area(.))
  }


  # transform to desired CRS
  if(!is.null(EPSG.chr)){
    grid.sf <- sf::st_transform(x = grid.sf, crs = as.numeric(EPSG.chr) )
  }

  # append an id to each cell
  grid.sf$sid <- paste0(seq_along(1:nrow(data.frame(grid.sf))))

  # select useful features
  grid.sf <- grid.sf %>% dplyr::select(ISO, NAME_0, NAME_1, sid, sf..st_make_grid.extent.sf..cellsize...res.num..n...c.500..500...)

  # rename geometry column
  names(grid.sf)[names(grid.sf) == "sf..st_make_grid.extent.sf..cellsize...res.num..n...c.500..500..."] <- "geometry"
  sf::st_geometry(grid.sf) <- "geometry"

  if(sf.bool == TRUE){
    grid.sf
  }else{
    grid.sp <- as(grid.sf, "Spatial")
  }
}

#' Build a sf object containing reclassified CLC data
#'
#' @author Thomas Goossens - pokyah.github.io
#' @param country_code.chr a character specifying the ISO contrycode. Ex : BE for belgium
#' @param NAME_1.chr a character specifying the NAME_1 value for lower than country level information
#' @param EPSG.chr A character specifying the EPSG code of the desired Coordiante Reference System (CRS)
#' @param EPSG.corine.chr A character specifying the EPSG code of the downloaded Corine Data
#' @path.corine.shapefile.chr A character specifying the path where th corine shapefiles resides
#' @return a sf data frame containing reclasssified corine land cover data
build_cover.sf.fun <- function(
  country_code.chr,
  NAME_1.chr,
  EPSG.chr,
  path.corine.shapefile.chr,
  EPSG.corine.chr){

  # Get country geometry first
  extent.sp <- raster::getData('GADM', country=country_code.chr, level=1)
  file.remove(list.files(pattern = "GADM_"))
  crs <- crs(extent.sp)

  if(!is.null(NAME_1.chr)){
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  }
  # reproject in the desired CRS
  extent.sp <- sp::spTransform(extent.sp, sp::CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == EPSG.chr)$prj4))

  # Download CORINE land cover for Belgium from http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-en.xml
  corine.sp <- maptools::readShapePoly(path.corine.shapefile.chr)

  # Define the CRS of corine land cover data
  # We know the crs from the metadata provided on the website http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-en.xml
  raster::crs(corine.sp) <- as.character(dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4)

  # Crop corine to extent
  corine.extent.sp <- raster::crop(corine.sp, extent.sp)

  # legend of corine
  download.file("http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1/corine-land-cover-classes-and/clc_legend.csv/at_download/file",
                destfile = "corine.legend.csv")
  legend <- read.csv(file = "corine.legend.csv", header = TRUE, sep = ",")
  file.remove("corine.legend.csv")

  # Legend codes present in extent
  legend.extent <- data.frame(unique(corine.extent.sp$code_12))

  # https://stackoverflow.com/questions/38850629/subset-a-column-in-data-frame-based-on-another-data-frame-list
  legend.extent <- subset(legend, CLC_CODE %in% legend.extent$unique.corine.extent.sp.code_12.)

  # CLC_CODE class from integer to numeric
  legend.extent$CLC_CODE <- as.numeric(legend.extent$CLC_CODE)

  # from sp to sf
  corine.extent.sf <- sf::st_as_sf(corine.extent.sp)
  corine.extent.sf$code_12 <- as.numeric(paste(corine.extent.sf$code_12))

  # Reclass Corine according to the following reclassification table
  cover.sf <-
    sf::st_as_sf(
      dplyr::mutate(
        corine.extent.sf,
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
}



#' Build a responsive leaflet map displaying agromet AWS network data
#' @author Thomas Goossens - pokyah.github.io
#' @param records.sf A sf containing the records to be displayed
#' @return a leaflet map object
#' @export
build_leaflet_template.fun <- function(records.sf){
  responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

  template.map <- leaflet::leaflet() %>%
    addProviderTiles(group = "Stamen",
                     providers$Stamen.Toner,
                     options = providerTileOptions(opacity = 0.25)
    ) %>%
    addProviderTiles(group = "Satellite",
                     providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 1)
    ) %>%
    fitBounds(sf::st_bbox(records.sf)[[1]],
              sf::st_bbox(records.sf)[[2]],
              sf::st_bbox(records.sf)[[3]],
              sf::st_bbox(records.sf)[[4]]
    ) %>%
    addLayersControl(baseGroups = c("Stamen", "Satellite"),
                     overlayGroups = c("KNMI rain radar", "stations", "MNT", "slope", "aspect"),
                     options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    htmlwidgets::onRender(paste0("
                                 function(el, x) {
                                 $('head').append(",responsiveness.chr,");
                                 }"))
  return(template.map)
}
