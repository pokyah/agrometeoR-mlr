#' Spatialize solar irradiance for an hour and return its result in a dataframe
#' @author Loïc Davadan - ldavadan.github.io
#' @param dssf.records.df A JSON file with records from API
#' @param hour.chr A character specifying which hour you want ; respect the format 'YYYY-MM-DD HH:MM:SS'
#' @param grid.pt.sp A SpatialPointsDataFrame grid with centroids
#' @return a dataframe or a sf (epsg 3812) of spatialized solar irradiance for an hour
#' @export
build.dssf.hour <- function(dssf.nested.df, hour.chr, grid.pt.sp) {
  
  # dssf.nested.df <- dssf.records.df %>%
  #   group_by(mhour) %>%
  #   tidyr::nest()
  
  dssf.hour.df <- subset(dssf.nested.df, mhour == hour.chr)
  
  dssf.hour.sp <- dssf.hour.df$data[[1]]
  sp::coordinates(dssf.hour.sp) <- ~lat+lon
  # set crs lambert 2008
  raster::crs(dssf.hour.sp) <- "+proj=longlat +datum=WGS84 +no_defs"
  dssf.hour.sp <- sp::spTransform(dssf.hour.sp,
                                 CRSobj ="+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # interpolate surface with IDW method (inverse distance weighted)
  idw <- gstat::krige(formula = ens ~ 1, locations = dssf.hour.sp, newdata = grid.pt.sp)  # apply idw model for the data
  idw.output.df <- as.data.frame(idw) %>% # output is defined as a data table
    dplyr::select(-var1.var)
  names(idw.output.df)[1:3] <- c("long", "lat", "ens.pred")  # give names to the modelled variables
  return(idw.output.df)
}



build.hourly.spatialized.dssf.df <- function(dssf.df, grid.pt.sp){
  
  if(!is.nan(dssf.df$ens) == TRUE){
    dssf.sp <- dssf.df
    sp::coordinates(dssf.sp) <- ~long+lat
    raster::crs(dssf.sp) <- "+proj=longlat +datum=WGS84 +no_defs"
    dssf.sp <- sp::spTransform(dssf.sp,
                               CRSobj ="+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    
    # interpolate surface with IDW method (inverse distance weighted)
    idw <- gstat::krige(formula = ens ~ 1, locations = dssf.sp, newdata = grid.pt.sp)  # apply idw model for the data
    idw.output.df <- as.data.frame(idw) %>% # output is defined as a data table
      dplyr::select(-var1.var)
    names(idw.output.df)[1:3] <- c("long", "lat", "ens.pred")  # give names to the modelled variables
    return(idw.output.df)
  }
}











