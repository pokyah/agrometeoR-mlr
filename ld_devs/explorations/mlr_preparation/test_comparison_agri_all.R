# load clc data
library(raster)
cover.sf <- build_cover.sf.fun(country_code.chr = "BE",
                               NAME_1.chr = "Wallonie", 
                               EPSG.chr = "3812", 
                               path.corine.shapefile.chr = "./external-data/Corine_Land_Cover/CLC12_BE.shp",
                               EPSG.corine.chr = "3812"
)
# keep agricultural areas and herbaceous areas
cover_agri_herb.sf <- cover.sf %>%
  dplyr::filter(CLASS == "Agricultural areas" | CLASS == "Herbaceous vegetation")

### spatialize prediction and error on the grid of Wallonia
load("./data/expl.static.grid.df.rda") # be careful it is a sf but it is more efficient (::todo:: modify generate independent variables)
# expl.static.grid.df <- data.frame(dplyr::bind_cols(expl.static.grid.df, data.frame(sf::st_coordinates(expl.static.grid.df))))
grid.1000.pt.sp <- build.vs.grid.fun(
  res.num = 1000,
  geom.chr = "centers",
  sf.bool = F,
  EPSG.chr = "3812",
  country_code.chr = "BE",
  NAME_1.chr = "Wallonie"
)

# get ens on virtual grid
load("./data/dssf.160101_160630.df.rda")
dssf.n.df <- dssf.160101_160630.df %>%
  group_by(mhour) %>%
  nest()

dates <- as.list(seq.POSIXt(from = as.POSIXct("2016-04-11 00:00:00"), to = as.POSIXct("2016-04-15 23:00:00"), by = "hour"))

dssf.pred.l <- lapply(dates, build.dssf.hour, dssf.nested.df = dssf.n.df, grid.pt.sp = grid.1000.pt.sp)

expl.static.grid.l <- lapply(dssf.pred.l, function(X){expl.static.grid.df$ens <- X$ens.pred})

expl.static.grid.df$ens <- dssf.pred.df$ens.pred



intpol.tsa.l <- lapply(dates, expl.static.grid.l, FUN = function(X,Y){
  spatialize(regr.lrn = makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.2bestsVar", predict.type = "se"), fw.method = "linear.correlation", fw.abs = 2),
             task = data.stations.n.df$tasks[[which(data.stations.n.df$mtime == X)]],
             prediction_grid.df = Y,
             predict.type = "se")
  })

intpol.tsa.sf <- spatialize(regr.lrn = makeFilterWrapper(learner = makeLearner(cl = "regr.lm", id = "lm.2bestsVar", predict.type = "se"), fw.method = "linear.correlation", fw.abs = 2),
                            task = data.stations.n.df$tasks[[which(data.stations.n.df$mtime == '2018-05-02 14:00:00')]],
                            prediction_grid.df = expl.static.grid.df,
                            predict.type = "se"
) %>%
  dplyr::select(gid, geometry, response, se)












intersect.clc_agri_herb.tsa_error.sf <- st_intersection(st_buffer(cover_agri_herb.sf, dist = 0), spatialized.tsa_error.pg.sf)

mean(spatialized.tsa_error.pg.sf$se, na.rm = TRUE)
mean(intersect.clc_agri_herb.tsa_error.sf$se, na.rm = TRUE)
