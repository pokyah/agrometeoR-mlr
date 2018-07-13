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

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

pal2 <- colorBin(palette = alphaPal("green"), domain= states$density, bins = bins, pretty = TRUE, na.color = "#808080",
                 alpha = TRUE, reverse = FALSE, right = FALSE)

load("~/Documents/code/agrometeor/agrometeoR-mlr/data/expl.static.stations.sf.rda")
load("~/Documents/code/agrometeor/agrometeoR-mlr/data/expl.static.grid.sf.rda")

stations.sf <- st_transform(expl.static.stations.sf, crs = 4326)
grid.sf <- st_transform(expl.static.grid.sf, crs = 4326)
grid.sp <- as(expl.static.grid.sf, "Spatial")
sp::gridded(grid.sp) = TRUE
#convert to raster
grid.r <- raster::raster(grid.sp)
#convert raster to polygons
grid.pg.sp = raster::rasterToPolygons(grid.r, dissolve = T)
grid.pg.sf <- st_as_sf(grid.pg.sp)
grid.pg.sf <- st_transform(grid.pg.sf, crs=4326)

joined.grid.sf <- st_join(grid.pg.sf, grid.sf)

grid.pg.sp@data
# https://stackoverflow.com/questions/20320377/how-to-change-a-spatialpointsdataframe-into-spatialpolygonsdataframe-in-r-to-use

stations.sp <- as(stations.sf, "Spatial")
extent.sp <- raster::getData('GADM', country="BE", level=1)
extent.sp <- subset(extent.sp, NAME_1 == "Wallonie")

crs <- crs(extent.sp)
# https://www.e-education.psu.edu/geog486/node/1891
leafletize <- function(records.sf, extent.sf){
  responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

  varPal <- colorNumeric(
    palette = "RdYlBu",
    domain = joined.grid.sf$altitude
  )
  uncPal <- colorBin(
    palette = alphaPal("#e6e6e6"),
    domain = joined.grid.sf$slope,
    bins = 5,
    alpha = TRUE
  )


  template.map <- leaflet::leaflet(test.sf) %>%
    addProviderTiles(group = "Stamen",
                     providers$Stamen.Toner,
                     options = providerTileOptions(opacity = 0.25)
    ) %>%
    addProviderTiles(group = "Satellite",
                     providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 1)
    ) %>%
    fitBounds(sf::st_bbox(test.sp)[[1]],
              sf::st_bbox(test.sp)[[2]],
              sf::st_bbox(test.sp)[[3]],
              sf::st_bbox(test.sp)[[4]]
    ) %>%
    addLayersControl(baseGroups = c("Stamen", "Satellite"),
                     overlayGroups = c("Terrain", "Stations", "Admin", "Uncertainty"),
                     options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    htmlwidgets::onRender(paste0("
                                 function(el, x) {
                                 $('head').append(",responsiveness.chr,");
                                 }")
    ) %>%
    addCircleMarkers(group = "Stations")%>%
    addPolygons(data = joined.grid.sf,
                group = "Terrain",
                color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.8,
                fillColor = ~varPal(altitude),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)
    )%>%
    addLegend(data = joined.grid.sf,
              position = "bottomright", pal = varPal, values = ~altitude,
              title = "Altitude (m)",
              group = "Terrain",
              opacity = 1
    )%>%
    addPolygons(data = joined.grid.sf,
                group = "Uncertainty",
                color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 1,
                fillColor = ~uncPal(slope),#~colorQuantile(alphaPal("#e6e6e6"), slope,alpha = TRUE)(slope),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)
    )%>%
    addLegend(data = joined.grid.sf,
              group = "Uncertainty",
              position = "bottomleft", pal = uncPal, values = ~slope,
              title = "uncertainty",
              opacity = 1
    )%>%
    addPolygons(data = extent.sp,
                group = "Admin",
                stroke = TRUE,
                fill = FALSE,
                weight = 1,
                color = "black"
    )

  library(htmltools)
  browsable(
    tagList(list(
      tags$head(
        tags$style(
          ".leaflet .legend i{
            border-radius: 50%;
            width:10px;
            height: 10px;
            margin-top: 4px;
         }
        "
        )
      ),
      leaf
    ))
  )
  return(template.map)
}

m <- leafletize(test.sp)


# From http://leafletjs.com/examples/choropleth/us-states.js
states <- geojsonio::geojson_read("./tg_devs/explorations/mapping/us-states.geojson", what = "sp")



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
