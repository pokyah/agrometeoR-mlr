library(sp)
sp.theme(TRUE)
data(meuse)
coordinates(meuse) = c("x", "y")
sel = spplot(meuse, "zinc", identify = TRUE)
sel

data(meuse.riv)
dim(meuse.riv)
meuse.riv[1:10, ]
meuse.sp = SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)), "meuse.riv")))
meuse.lt = list("sp.polygons", meuse.sp, fill = "grey")

# TREND SURFACES
library(gstat)
data(meuse.grid)
coordinates(meuse.grid) = ~x + y
gridded(meuse.grid) = TRUE
meuse.grid$tr1 = krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 1)$var1.pred
meuse.grid$tr2 = krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 2)$var1.pred
meuse.grid$tr3 = krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 3)$var1.pred
spplot(meuse.grid, c("tr1", "tr2", "tr3"), sp.layout = meuse.lt, main = "log(zinc), trend surface interpolation")

# IDW
# https://rstudio-pubs-static.s3.amazonaws.com/63374_8651f7cd6b2d41a5bba5708d2b40f24e.html
library(gstat)
lzn.tp = idw(log(zinc) ~ 1, meuse, meuse.grid)
spplot(lzn.tp, "var1.pred", sp.layout = meuse.lt, main = "log(zinc), inverse distance interpolation")
meuse.grid$idp0.5 = idw(log(zinc) ~ 1, meuse, meuse.grid, idp = 0.5)$var1.pred
meuse.grid$idp02 = idw(log(zinc) ~ 1, meuse, meuse.grid, idp = 2)$var1.pred
meuse.grid$idp05 = idw(log(zinc) ~ 1, meuse, meuse.grid, idp = 5)$var1.pred
meuse.grid$idp10 = idw(log(zinc) ~ 1, meuse, meuse.grid, idp = 10)$var1.pred
spplot(meuse.grid, c("idp0.5", "idp02", "idp05", "idp10"), sp.layout = meuse.lt, main = "log(zinc), inverse distance interpolation")
