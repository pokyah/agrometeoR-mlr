extent.sp <- raster::getData('GADM', country="BE", level=1, path =  "./external-data/Boundaries")

extent.sp <- subset(extent.sp, NAME_1 == "Wallonie")
extent.sf <- sf::st_as_sf(extent.sp)

extent.sf <- sf::st_transform(extent.sf, crs = 3812 )


a = raster(as(extent.sf, "Spatial")); res(a)=c(1000,1000)


# Crop tiles to extent borders
a <- raster::crop(a, as(extent.sf, "Spatial"))
a <- raster::mask(a, as(extent.sf, "Spatial"))


