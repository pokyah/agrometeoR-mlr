library(mlr)
class(meuse)
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

# http://rspatial.org/analysis/rst/4-interpolation.html
