source("./R/geo.R")
vs.grid.1km.wallonia.polygons_sf <- build.vs.grid.fun(
  res.num = 1000,
  geom.chr = "polygons",
  sf.bool = TRUE,
  EPSG.chr = "3812",
  country_code.chr = "BE",
  NAME_1.chr = "Wallonie"
  )
devtools::use_data(vs.grid.1km.wallonia.polygons_sf, overwrite = TRUE)

# rgdal::writeOGR(as(wal_grid_1km.sf, "Spatial"), dsn = './', layer = 'wal_poly_grid', driver = "ESRI Shapefile")
