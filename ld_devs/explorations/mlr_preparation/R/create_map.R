### Declaration of the function to create the map of spatialized temperature

#' It creates a map with spatialized data
#' @author Loïc Davadan - ldavadan.github.io
#' @param spatial_data.sf A sf with spatialized data
#' @param type.chr A character which specifies if you want a static or an interactive map.The arguments accepted are "static", "interactive"
#' @param method.chr A character which specifies the method of spatialization you used and that will be printed on your map
#' @param date.chr A character which contains the date you want to visualize for the title of the map
#' @param country_code.chr A character containing the GADM code of the country you want to get boundaries
#' @param NAME_1.chr A character which specifies the region you want, use GADM nomenclature
#' @param error.bool A boolean which specifies if you want to visualize error on the map or not
#' @param error_layer.bool A boolean which specifies if you want to get the layer containing standard error
#' @param alpha_error.num A numeric specifying transparency of the layer containing standard error
#' @return a map, static or interactive with spatialized data
create_map_tsa <- function(
  spatial_data.sf,
  method.chr,
  date.chr,
  type.chr,
  country_code.chr,
  NAME_1.chr,
  error.bool,
  error_layer.bool,
  alpha_error.num = NULL){
  
  
  library(tmap)
  library(tmaptools)
  library(RColorBrewer)
  
  if(error.bool == TRUE){
    
    spatial_data.sp <- as(spatial_data.sf, "Spatial")

    # file <- "./fig/craw.png"
    # choose boundaries to display
    extent.sp <- raster::getData('GADM', country=country_code.chr, level=1, path = "./external-data/Boundaries")
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  
    # build the map
    static <- tmap::tm_shape(spatial_data.sp, projection="3812") +            # Projection : Belgian Lambert
      tm_fill(col = "response",
              popup.vars = c("response", "se"),
              title = "Temperature (°C)",
              palette = "-RdYlBu",
              auto.palette.mapping = FALSE,
              lwd = 0,
              popup.format = list(digits = 3),
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
      tmap::tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
      tmap::tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar 
                   color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
                   just = NA) +
      # tmap::tm_logo(file, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
      #         position = c(-0.01,-0.04), just = NA) +
      tmap::tm_shape(extent.sp) +                                      # boundaries
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
                main.title.size = 0.9,
                title = "Resolution : 1 km²", 
                title.size = 0.6, 
                title.position = c(0.01, 0.96)) +
      tmap::tm_credits("© CRA-W", position = c(.87, 0))
    
    if(error_layer.bool == TRUE){
      
      spatial_error.sp <- spatial_data.sp
      static <- static +
        tm_shape(spatial_error.sp, is.master = TRUE) +
        tm_fill("se",
                palette = whiteAlpha(),
                auto.palette.mapping = FALSE,
                title = "Standard error")
                # breaks = c(stats::quantile(spatial_error.sp$se, 0, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.1, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.2, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.3, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.4, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.5, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.6, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.7, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.8, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 0.9, na.rm = TRUE),
                #            stats::quantile(spatial_error.sp$se, 1, na.rm = TRUE)))
    }
    
    if(type.chr == "static") {
      return(static)
    }else{
      interactive <- tmap::tmap_leaflet(static)
      return(interactive)
    }
  }else{
    
    spatial_data.sp <- as(spatial_data.sf, "Spatial") %>%
      as(., "SpatialPixelsDataFrame") %>%
      as(., "SpatialGridDataFrame")
    
    # # define response minimum, response maximum, and response difference
    # min_resp = round(min(spatial_data.sp@data$response, na.rm = TRUE), digits = 2)
    # max_resp = round(max(spatial_data.sp@data$response, na.rm = TRUE), digits = 2)
    # int_resp = round(((max_resp - min_resp)/10), digits = 2)
    # # create a vector with legend breaks
    # legend_break = c(min_resp,
    #                  min_resp + int_resp,
    #                  min_resp + 2*int_resp,
    #                  min_resp + 3*int_resp,
    #                  min_resp + 4*int_resp,
    #                  min_resp + 5*int_resp,
    #                  min_resp + 6*int_resp,
    #                  min_resp + 7*int_resp,
    #                  min_resp + 8*int_resp,
    #                  min_resp + 9*int_resp,
    #                  max_resp)
    
    # file <- "./fig/craw.png"
    extent.sp <- raster::getData('GADM', country=country_code.chr, level=1, path = "./external-data/Boundaries")
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
    
    static <- tmap::tm_shape(spatial_data.sp, projection="3812")  +           # Projection : Belgian Lambert
        tmap::tm_raster("response",                                            # spatialize temperature
                        palette = "-RdYlBu",
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
        tmap::tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
        tmap::tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar 
                           color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
                           just = NA) +
        # tmap::tm_logo(file, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
        #         position = c(-0.01,-0.04), just = NA) +
        tmap::tm_shape(extent.sp) +                                      # outline of Wallonia
        tmap::tm_borders("grey20", lwd = 1.5) +
        tmap::tm_layout(legend.position = c(0.01,0.14),                        # parameters
                        legend.height = 0.55,
                        legend.text.size = 0.7,
                        legend.bg.color = "white",
                        legend.title.size = 0.9,
                        inner.margins = c(0.03, 0.03, 0.07, 0.03),
                        frame.lwd = 0,
                        bg.color = "grey85",
                        main.title = base::paste("Interpolated temperature with ", method.chr, " - ", date.chr),
                        main.title.size = 0.9,
                        title = "Resolution : 1 km²", 
                        title.size = 0.6, 
                        title.position = c(0.01, 0.96)) +
        tmap::tm_credits("© CRA-W", position = c(.87, 0))
    
    if(error_layer.bool == TRUE){
      
      spatial_error.sp <- spatial_data.sp
      static <- static +
        tm_shape(spatial_error.sp, is.master = TRUE) +
        tm_raster("se",
                  alpha = alpha_error.num,
                  saturation = 0,
                  title = "Standard error",
                  breaks = c(stats::quantile(spatial_error.sp$se, 0, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.1, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.2, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.3, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.4, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.5, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.6, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.7, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.8, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 0.9, na.rm = TRUE),
                             stats::quantile(spatial_error.sp$se, 1, na.rm = TRUE)))
    }
    
    if(type.chr == "static") {
      return(static)
      }else{
        interactive <- tmap::tmap_leaflet(static)
        return(interactive)
      }
  }
}

#' It creates a map with spatialized data and its related error
#' @author Loïc Davadan - ldavadan.github.io
#' @param spatial_data.sf A sf with spatialized data
#' @param type.chr A character which specifies if you want a static or an interactive map. The arguments accepted are "static", "interactive"
#' @param method.chr A character which specifies the method of spatialization you used and that will be printed on your map
#' @param date.chr A character which contains the date you want to visualize for the title of the map
#' @param country_code.chr A character containing the GADM code of the country you want to get boundaries
#' @param NAME_1.chr A character which specifies the region you want, use GADM nomenclature
#' @param overlay.bool A boolean which specifies if you want to overlay layers
#' @return a map, static or interactive with spatialized data
make.map.tsa_prediction.fun <- function(
  spatial.data.sf,
  type.chr,
  method.chr,
  date.chr,
  country_code.chr,
  NAME_1.chr,
  overlay.bool
){
  
  library(tmap)
  
  spatial.data.sp <- as(spatial.data.sf, "Spatial") %>%
    as(., "SpatialPixelsDataFrame") %>%
    as(., "SpatialGridDataFrame")
  
  # file <- "./fig/craw.png"
  extent.sp <- raster::getData('GADM', country=country_code.chr, level=1, path = "./external-data/Boundaries")
  extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
  
  if(overlay.bool == FALSE){
  
    static <- tmap::tm_shape(spatial.data.sp, projection="3812")  +           # Projection : Belgian Lambert
      tmap::tm_raster(c("response", "se"),                                            # spatialize temperature
                      palette = list("-RdYlBu", "Greys"),
                      title = c("Temperature (°C)","Standard error"),
                      auto.palette.mapping=FALSE,
                      breaks = list(c(stats::quantile(spatial.data.sp$response, 0, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.1, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.2, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.3, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.4, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.5, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.6, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.7, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.8, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 0.9, na.rm = TRUE),
                                 stats::quantile(spatial.data.sp$response, 1, na.rm = TRUE)),
                                 c(stats::quantile(spatial.data.sp$se, 0, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.1, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.2, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.3, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.4, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.5, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.6, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.7, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.8, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 0.9, na.rm = TRUE),
                                   stats::quantile(spatial.data.sp$se, 1, na.rm = TRUE)))) +
      tmap::tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
      tmap::tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar 
                         color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
                         just = NA) +
      # tmap::tm_logo(file, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
      #         position = c(-0.01,-0.04), just = NA) +
      tmap::tm_shape(extent.sp) +                                      # outline of Wallonia
      tmap::tm_borders("grey20", lwd = 1.5) +
      tmap::tm_layout(legend.position = c(0.01,0.14),                        # parameters
                      legend.height = 0.55,
                      legend.text.size = 0.7,
                      legend.bg.color = "white",
                      legend.title.size = 0.9,
                      inner.margins = c(0.03, 0.03, 0.07, 0.03),
                      frame.lwd = 0,
                      bg.color = "grey85",
                      main.title = paste("Interpolated temperature with ", method.chr, " - ", date.chr),
                      main.title.size = 0.9,
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
  }else{
    
    spatial.error.sp <- spatial.data.sp
    
    static <- tmap::tm_shape(spatial.data.sp, projection="3812")  +           # Projection : Belgian Lambert
      tmap::tm_raster("response",                                            # spatialize temperature
                      palette = "-RdYlBu",
                      title = "Temperature (°C)",
                      auto.palette.mapping=FALSE,
                      breaks = c(stats::quantile(spatial.data.sp$response, 0, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.1, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.2, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.3, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.4, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.5, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.6, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.7, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.8, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 0.9, na.rm = TRUE),
                                      stats::quantile(spatial.data.sp$response, 1, na.rm = TRUE))) +
      tm_shape(spatial.error.sp, is.master = TRUE) +
      tm_raster("se",
                saturation = 0,
                alpha = 0.7,
                title = "Standard error",
                auto.palette.mapping = FALSE,
                breaks = c(stats::quantile(spatial.error.sp$se, 0, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.1, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.2, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.3, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.4, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.5, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.6, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.7, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.8, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 0.9, na.rm = TRUE),
                           stats::quantile(spatial.error.sp$se, 1, na.rm = TRUE))) +
      tmap::tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
      tmap::tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar 
                         color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
                         just = NA) +
      # tmap::tm_logo(file, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
      #         position = c(-0.01,-0.04), just = NA) +
      tmap::tm_shape(extent.sp) +                                      # outline of Wallonia
      tmap::tm_borders("grey20", lwd = 1.5) +
      tmap::tm_layout(legend.position = c(0.01,0.14),                        # parameters
                      legend.height = 0.55,
                      legend.text.size = 0.7,
                      legend.bg.color = "white",
                      legend.title.size = 0.9,
                      inner.margins = c(0.03, 0.03, 0.07, 0.03),
                      frame.lwd = 0,
                      bg.color = "grey85",
                      main.title = paste("Interpolated temperature with ", method.chr, " - ", date.chr),
                      main.title.size = 0.9,
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
}



create_map_tsa.comparison_clc <- 
  function(spatial_data.sf,
           clc.sf,
           country_code.chr,
           NAME_1.chr){
    
    
    spatial_data.sp <- as(spatial_data.sf, "Spatial") %>%
      as(., "SpatialPixelsDataFrame") %>%
      as(., "SpatialGridDataFrame")
    
    # # define response minimum, response maximum, and response difference
    # min_resp = round(min(spatial_data.sp@data$response, na.rm = TRUE), digits = 2)
    # max_resp = round(max(spatial_data.sp@data$response, na.rm = TRUE), digits = 2)
    # int_resp = round(((max_resp - min_resp)/10), digits = 2)
    # # create a vector with legend breaks
    # legend_label = c(min_resp, "",
    #                  min_resp + int_resp, "",
    #                  min_resp + 2*int_resp, "",
    #                  min_resp + 3*int_resp, "",
    #                  min_resp + 4*int_resp, "",
    #                  min_resp + 5*int_resp, "",
    #                  min_resp + 6*int_resp, "",
    #                  min_resp + 7*int_resp, "",
    #                  min_resp + 8*int_resp, "",
    #                  min_resp + 9*int_resp, "",
    #                  max_resp)
    
    # file <- "./fig/craw.png"
    # choose boundaries to display
    extent.sp <- raster::getData('GADM', country=country_code.chr, level=1, path = "./external-data/Boundaries")
    extent.sp <- subset(extent.sp, NAME_1 == NAME_1.chr)
    
    # build the map
    static <- tmap::tm_shape(spatial_data.sp, projection="3812") +            # Projection : Belgian Lambert
      tm_raster(col = "se",
                saturation = 0,
                breaks = c(0, 0.2, 0.4, 0.6, 0.7)) +
      tm_shape(clc.sf) +
      tm_fill(col = "red", alpha = 0.2) +
      tmap::tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
      tmap::tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar 
                         color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
                         just = NA) +
      # tmap::tm_logo(file, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
      #         position = c(-0.01,-0.04), just = NA) +
      tmap::tm_shape(extent.sp) +                                      # boundaries
      tmap::tm_borders("grey20", lwd = 1.5) +
      tmap::tm_layout(legend.position = c(0.01,0.14),                         # parameters
                      legend.height = 0.55,
                      legend.text.size = 0.7,
                      legend.bg.color = "white",
                      legend.title.size = 0.9,
                      inner.margins = c(0.03, 0.03, 0.07, 0.03),
                      frame.lwd = 0,
                      bg.color = "grey85",
                      title = "Resolution : 1 km²", 
                      title.size = 0.6, 
                      title.position = c(0.01, 0.96)) +
      tmap::tm_credits("© CRA-W", position = c(.87, 0))
    
    interactive <- tmap_leaflet(static)
    
    

    
  }
