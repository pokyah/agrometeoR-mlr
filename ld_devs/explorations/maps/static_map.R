#' Build a static map displaying predictions and their related error
#' @author Loïc Davadan <- ldavadan.github.io
#' @param gridded.data.df A data frame obtained from a SpatialGridDataFrame containing data
#' @param boundaries.sf A sf containing data from Wallonia boundaries
#' @param layer.error.bool A boolean specifying if you want to display the layer with error
#' @param legend.error.bool A boolean specifying if you want to display the legend of the error layer
#' @param pretty_breaks.bool A boolean specifying the type of legend you want. TRUE for pretty breaks, FALSE for quantile scale
#' @param title.chr A character specifying the title you want for your map
#' @param target.chr A character specifying the predicted parameter. One of "tsa", "hra" or "hct"
#' @return a ggplot map object
#' @export
static.ggmap <- function(
  gridded.data.df,
  boundaries.sf,
  layer.error.bool,
  legend.error.bool,
  pretty_breaks.bool,
  title.chr,
  target.chr
){
  
  library(ggplot2)
  library(grid)
  library(maps)
  library(maptools)
  library(ggsn)
  library(RColorBrewer)
  
  if(target.chr == "tsa"){ legend_title = "Temperature (°C)"}
  if(target.chr == "hra"){ legend_title = "Relative humidity (%)"}
  if(target.chr == "hct"){ legend_title = "Leaves wetness (%)"}
  
  if(pretty_breaks.bool == TRUE){
    # inspired by https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
    # prepare legend with pretty breaks
    # compute quantiles from predictions values
    quantiles <- unique(stats::quantile(gridded.data.df$response,
                                 probs = seq(0, 1, length.out = 11), na.rm=T))
    labels <- c()
    breaks <- unique(round(c(-60,
                             min(gridded.data.df$response, na.rm = TRUE),
                             quantiles,
                             max(gridded.data.df$response, na.rm = TRUE)), 1))
    
    labels <- paste0(labels, paste0(format(round(breaks, 1), nsmall = 1)))
    labels <- labels[2:length(labels)]
    gridded.data.df$response_quantiles <- cut(gridded.data.df$response, 
                                              breaks = breaks, 
                                              labels = labels, 
                                              include.lowest = T)
    breaks_scale <- levels(gridded.data.df$response_quantiles)
    labels_scale <- rev(breaks_scale)
  }
  if(pretty_breaks.bool == FALSE){
    # inspired by https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
    quantiles <- unique(stats::quantile(gridded.data.df$response,
                                 probs = seq(0, 1, length.out = 11), na.rm=T))
    labels <- c()
    labels <- paste0(labels, paste0(format(round(quantiles, 1), nsmall = 1),
                                    " – ",
                                    format(round(quantiles[2:length(quantiles)], 1), nsmall = 1)))
    labels <- labels[1:length(labels)-1]
    gridded.data.df$response_quantiles <- cut(gridded.data.df$response,
                                                       breaks = quantiles,
                                                       labels = labels,
                                                       include.lowest = T)
  }
  
  ggmap <- ggplot2::ggplot(gridded.data.df) +
    # choose data to display on the layer
    ggplot2::geom_raster(mapping = ggplot2::aes(coords.x1, coords.x2, fill = response_quantiles), na.rm = TRUE, interpolate = T)
  # choose color palette and create a legend with pretty breaks
  if(pretty_breaks.bool == TRUE){
    ggmap <- ggmap +
      ggplot2::scale_fill_manual(
        values = rev(RColorBrewer::brewer.pal(n = length(labels_scale), name = "RdYlBu")), # palette to use
        breaks = rev(breaks_scale), # legend breaks
        name = legend_title,
        drop = FALSE,
        labels = labels_scale, # legend labels
        # legend parameters
        guide = ggplot2::guide_legend(
          direction = "vertical",
          keyheight = grid::unit(7, units = "mm"),
          keywidth = grid::unit(3, units = "mm"),
          title.position = 'top',
          title.vjust = 0.5,
          label.vjust = 1,
          ncol = 1,
          bycol = T,
          reverse = F,
          label.position = "right"
        )
      )
  }
  # color palette with discrete classes with quantile scale
  if(pretty_breaks.bool == FALSE){
    ggmap <- ggmap +
      ggplot2::scale_fill_brewer(legend_title, palette = "RdYlBu", direction = -1)
  }
    
  if(layer.error.bool == TRUE){
    ggmap <- ggmap + 
      # display a layer with standard error
      ggplot2::geom_raster(ggplot2::aes(coords.x1, coords.x2, alpha = se), fill = "white", na.rm = TRUE, interpolate = TRUE) +
      # whitening it
      ggplot2::scale_alpha_continuous("Standard\nError",range = c(0.1,1), guide = legend.error.bool)
      # order the two legends if they both are displayed
      if(legend.error.bool == TRUE){
        ggmap <- ggmap + ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                                alpha = ggplot2::guide_legend(order = 0))
      }
      
  }
  ggmap <- ggmap +
    ggplot2::ggtitle(title.chr) +   # add title
    # add boundaries layer
    ggplot2::geom_sf(data = boundaries.sf, ggplot2::aes(fill = ISO), fill = NA, color = "black", size = 0.6) +
    # add north symbol
    ggsn::north(boundaries.sf, scale = 0.1, location = "bottomleft",
                anchor = c(x = 780000, y = 550000), symbol = 12) +
    # add scalebar
    ggsn::scalebar(boundaries.sf, dist = 50, dd2km = FALSE, model = "GRS80", 
                   st.dist = 0.03, st.size = 4, box.fill = c("black", "white"),
                   box.color = "black", anchor = c(x = 700000, y = 520000)) +
    # add copyright
    ggplot2::annotation_custom(grob = grid::textGrob("© CRA-W"),
                      xmin = 790000, xmax = 790000, ymin = 520000, ymax = 520000) +
    # display resolution of the map
    ggplot2::annotation_custom(grob = grid::textGrob("Resolution : 1 km²"),
                      xmin = 558000, xmax = 558000, ymin = 671000, ymax = 671000) +
    # parameters for visualization
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
          axis.title = ggplot2::element_text(color = NA),
          panel.grid = ggplot2::element_line(color = NA),
          axis.ticks = ggplot2::element_line(color = NA),
          axis.text = ggplot2::element_text(colour = NA),
          legend.title = ggplot2::element_text(size = 12, face = "bold", vjust = 1),
          legend.text = ggplot2::element_text(size = 11, margin(b = 1)),
          legend.background = ggplot2::element_rect(fill = "transparent"),
          legend.position = c(0.12,0.38),
          legend.box = "horizontal")
  ggmap
}

