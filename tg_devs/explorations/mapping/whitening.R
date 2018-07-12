library(leaflet)
library(dplyr)
library(geojsonio)

alphaPal <- function(color) {
  alpha <- seq(0,1,0.1)
  r <- col2rgb(color, alpha=T)
  r <- t(apply(r, 1, rep, length(alpha)))
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
  return(codes)
}

load("~/Documents/code/agrometeor/agrometeoR-mlr/data/expl.static.stations.sf.rda")

leafletize(expl.static.stations.sf)

leafletize <- function(records.sf){
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
                     overlayGroups = c("slope", "altitude"),
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








# From http://leafletjs.com/examples/choropleth/us-states.js
states <- geojsonio::geojson_read("./tg_devs/explorations/mapping/us-states.geojson", what = "sp")

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

pal2 <- colorBin(palette = alphaPal("green"), domain= states$density, bins = bins, pretty = TRUE, na.color = "#808080",
         alpha = TRUE, reverse = FALSE, right = FALSE)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal2(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal2, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")
