library(mlr)
library(sp)
library(gstat)
data(meuse)
class(meuse)

# theory combined with R
# https://www.r-project.org/conferences/DSC-2003/Proceedings/Pebesma.pdf
# https://www.researchgate.net/publication/266418957_A_minimal_introduction_to_geostatistics_with_Rgstat
# https://www.sciencedirect.com/science/article/pii/S0341816213002385

# understanding the different types of kriging
# http://r-sig-geo.2731867.n2.nabble.com/gstat-krige-regression-kriging-vs-kriging-with-external-drift-td7589206.html

# implementing to mlr
# https://mlr-org.github.io/mlr/articles/tutorial/devel/create_learner.html
# https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html
# https://github.com/mlr-org/mlr/blob/master/R/RLearner_regr_km.R
# https://www.rdocumentation.org/packages/DiceKriging/versions/1.5.5/topics/km

# the gstat krige function actaully build the object and makes the predictions. We need to decompose it !
# this one is the function to be used
# https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/gstat
# autofitting the variogram https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html
# https://cran.r-project.org/web/packages/automap/automap.pdf
# http://spatial-analyst.net/wiki/index.php/Best_Combined_Spatial_Prediction
# http://pebesma.staff.ifgi.de/modellierung/course.pdf

# Automap might be the good bet for autofittig vgm model
# https://cran.r-project.org/web/packages/automap/automap.pdf
# http://www.numbertheory.nl/2013/02/17/automatic-spatial-interpolation-with-r-the-automap-package/
# actually not useful anymore since gstat can also perform autofitting of vg https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html


# Starting point for test. We need multivariate !
# https://rstudio-pubs-static.s3.amazonaws.com/63374_8651f7cd6b2d41a5bba5708d2b40f24e.html
# https://rpubs.com/nabilabd/118172 ==> PERFECT begining point !
# https://www.stat.berkeley.edu/~arturof/Teaching/STAT248/lab10_part2.html



library(sp)
library(mlr)
library(dplyr)
data(meuse)
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE
# coordinates(meuse) = ~x+y
#meuse <- meuse %>% select(c("x", "y", "zinc"))
meuse <- meuse %>% select(c("x", "y", "zinc", "dist", "soil", "ffreq"))
meuse <- meuse %>% dplyr::mutate(log_zinc = log(zinc))
meuse <- meuse[complete.cases(meuse),]

meuse <- meuse %>% select(c("x", "y", "log_zinc"))
task = makeRegrTask(id = "meuse",  data = meuse, target = "log_zinc") #,coordinates = meuse[c("x","y")])
lrn.idw = makeLearner(cl = "regr.gstat", id= "idw", locations = ~x+y)
mod.idw = train(lrn.idw, task)
newdata.pred.idw = predict(mod.idw, newdata = data.frame(meuse.grid))
res.idw <- bind_cols(data.frame(meuse.grid), newdata.pred.idw$data)
coordinates(res.idw) <- ~x+y
spplot(res.idw["response"], do.log = T, colorkey = TRUE, main = mod.idw$learner$id)


data(meuse)
meuse <- meuse %>% select(c("x", "y", "zinc", "dist", "soil", "ffreq"))
meuse <- meuse %>% dplyr::mutate(log_zinc = log(zinc))
meuse <- meuse %>% select(c("x", "y", "zinc"))
task = makeRegrTask(id = "meuse",  data = meuse, target = "zinc") #,coordinates = meuse[c("x","y")])
lrn.vgm = makeLearner(cl = "regr.gstat", id= "vgm", model = list(psill=c("Sph","Exp","Gau", "Mat")), locations = ~x+y) #autofit vgm
mod.vgm = train(lrn.vgm, task)
#task.pred.vgm = mlr::predict(mod.vgm, task = task)
newdata.pred.vgm = predict(object = mod.vgm, newdata = data.frame(meuse.grid))
res.vgm <- bind_cols(data.frame(meuse.grid), newdata.pred.vgm$data)
coordinates(res.vgm) <- ~x+y
spplot(res.vgm["response"], do.log = T, colorkey = TRUE, main = mod.vgm$learner$id)

data(meuse)
meuse <- meuse %>% select(c("x", "y", "zinc", "dist", "soil", "ffreq"))
meuse <- meuse %>% dplyr::mutate(log_zinc = log(zinc))
meuse <- meuse %>% select(c("x", "y", "log_zinc"))
task = makeRegrTask(id = "meuse",  data = meuse, target = "log_zinc") #,coordinates = meuse[c("x","y")])
lrn.vgm_log = makeLearner(cl = "regr.gstat", id= "vgm_log", model = list(psill=1, model="Sph", range=900, nugget=1), locations = ~x+y)
mod.vgm_log = train(lrn.vgm_log, task)
newdata.pred.vgm_log = predict(object = mod.vgm_log, newdata = data.frame(meuse.grid))
res.vgm_log <- bind_cols(data.frame(meuse.grid), newdata.pred.vgm_log$data)
coordinates(res.vgm_log) <- ~x+y
spplot(res.vgm_log["response"], do.log = T, colorkey = TRUE, main = mod.vgm_log$learner$id)



# lrn.lm = makeLearner(cl = "regr.lm", id= "lm")
# lrn.elmNN = makeLearner(cl = "regr.elmNN", id= "elmNN", par.vals = list(actfun="sin"))
rdesc = makeResampleDesc("CV", iters = 20)
r = resample(learner = lrn.gstat, task = task, resampling = rdesc )
