### Declaration of the function to create the map of spatialized temperature

#' It creates a map with spatialized data
#' @author Loïc Davadan - ldavadan.github.io
#' @param spatial_data.sp A sp with spatialized data
#' @param type.chr A character which specifies if you want a static or an interactive map.The arguments accepted are "static", "interactive"
#' @param method.chr A character which specifies the method of spatialization you used and that will be printed on your map
#' @return a map, static or interactive with spatialized data
create_map_tsa <- function(
  spatial_data.sp,
  grid.sp,
  method.chr,
  date.chr,
  type.chr,
  country_code.chr,
  NAME_1.chr){
  
  
  base::library(tmap)


  # file <- "./fig/craw.png"
  extent.sp <- raster::getData('GADM', country=country_code.chr, level=1, path = "./external-data/Boundaries")
  extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  
  static <- tmap::tm_shape(spatial_data.sp, projection="3812") +            # Projection : Belgian Lambert
    tmap::tm_raster("response",                                             # spatialize temperature
              palette = "-Spectral",
              title = "Temperature (°C)",
              auto.palette.mapping=FALSE,
              breaks = c(stats::quantile(spatial_data.sp$response, 0, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.1, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.2, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.3, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.4, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.5, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.6, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.7, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.8, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 0.9, na.rm = TRUE),
                         stats::quantile(spatial_data.sp$response, 1, na.rm = TRUE))) +
    tm_shape(spatial_data.sp, is.master = TRUE) +
    tm_raster("se",
              alpha = 0.5,
              saturation = 0,
              title = "Standard error",
              breaks = c(0, 0.2, 0.4, 0.6, 0.7)) +
    tmap::tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
    tmap::tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar 
                 color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
                 just = NA) +
    # tmap::tm_logo(file, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
    #         position = c(-0.01,-0.04), just = NA) +
    tmap::tm_shape(extent.sp) +                                      # outline of Wallonia
    tmap::tm_borders("grey20", lwd = 1.5) +
    tmap::tm_layout(legend.position = c(0.01,0.14),                         # parameters
              legend.height = 0.55,
              legend.text.size = 0.7,
              legend.bg.color = "white",
              legend.title.size = 0.9,
              inner.margins = c(0.03, 0.03, 0.07, 0.03),
              frame.lwd = 0,
              bg.color = "grey85",
              main.title = base::paste("Interpolated temperature with ", method.chr, " - ", date.chr),
              title = "Resolution : 1 km²", 
              title.size = 0.6, 
              title.position = c(0.01, 0.96)) +
    tmap::tm_credits("© CRA-W", position = c(.87, 0))
  
  if(type.chr == "static") {
    return(static)
  }else{
    interactive <- tmap::tmap_leaflet(static)
    return(interactive)
  }
}


### Creation of the map but not in a function
# library(tmap)
# load("~/Documents/code/test/api_test/gridded.3812.sp")
# load("~/Documents/code/test/api_test/wallonie.3812.sp")
# file <- "./craw.png"
# map1 <- tm_shape(gridded.3812.sp, projection="3812") +
#   tm_raster("response",  
#             palette = "-RdYlBu",
#             title="Temperature (°C)",
#             auto.palette.mapping=FALSE,
#             breaks = breaks_legend(gridded.3812.sp)) +
#   tm_compass(position = c(0.9,0.15), color.light = "grey20") +
#   tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",
#                color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
#                just = NA) +
#   tm_logo(file, height = 2, halign = "center", margin = 0.2,
#           position = c(-0.01,-0.04), just = NA) +
#   tm_shape(wallonie.3812.sp) +
#   tm_borders("grey20", lwd = 1.5) +
#   tm_layout(legend.position = c(0.01,0.14),
#             legend.height = 0.55,
#             legend.text.size = 0.7,
#             legend.bg.color = "white",
#             legend.title.size = 0.9,
#             inner.margins = c(0.03, 0.03, 0.07, 0.03),
#             frame.lwd = 0,
#             bg.color = "grey85",
#             main.title = "Interpolated temperature with <method>",
#             title = "Resolution : 1 km²", 
#             title.size = 0.6, 
#             title.position = c(0.01, 0.96)) +
#   tm_credits("© CRA-W", position = c(.87, 0))
# map1
# map <- tmap_leaflet(map1)
# map


# load("~/Documents/code/agromet-tests/data-raw/gridded.3812.sp")
# create_map_tsa(spatial_data.sp = gridded.3812.sp, type.chr = "static", method.chr = "<na>")

