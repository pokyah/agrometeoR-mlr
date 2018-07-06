#' @export
makeRLearner.regr.gstat = function(){#https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/fit.variogram
  makeRLearnerRegr(
    cl = "regr.gstat",
    package = "gstat",
    par.set = makeParamSet(#https://www.rdocumentation.org/packages/ParamHelpers/versions/1.8/topics/LearnerParam
      makeFunctionLearnerParam(id = "g"),
      makeUntypedLearnerParam(id = "id"),#FIXME
      makeUntypedLearnerParam(id = "locations"),#FIXME
      makeUntypedLearnerParam(id = "model", default = NULL),#FIXME what should be the type
      makeIntegerVectorLearnerParam(id = "beta"),
      makeIntegerLearnerParam(id = "nmax", default = 0),
      makeIntegerLearnerParam(id = "nmin", default = 0),
      makeIntegerLearnerParam(id = "omax", default = 0),
      makeNumericLearnerParam(id = "maxdist", default = 100000000000),#FIXME should be Inf
      makeLogicalLearnerParam(id = "force", default = FALSE),
      makeLogicalLearnerParam(id = "dummy", default = FALSE),
      makeUntypedLearnerParam(id = "set"),
      makeFunctionLearnerParam(id = "x"),
      makeLogicalLearnerParam(id = "fill.all", default = FALSE),
      makeLogicalLearnerParam(id = "fill.cross", default = TRUE),
      makeDiscreteLearnerParam(id = "variance", values = c("identity", "mu", "mu(1-mu)"), default = "identity"),
      #makeNumericVectorLearnerParam(id = "weights", default = NULL),#FIXME
      makeUntypedLearnerParam(id = "merge"),
      makeIntegerLearnerParam(id = "degree", default = 0),
      makeLogicalLearnerParam(id = "vdist", default = FALSE),
      makeUntypedLearnerParam(id = "lambda")
    ),
    par.vals = list(model = NULL, nmax = 0, nmin=0, omax=0, maxdist=Inf, force=FALSE, dummy=FALSE, fill.all=FALSE, fill.cross=TRUE, variance="identity", degree=0, vdist=FALSE),
    properties = c("numerics", "factors" , "se", "weights"),
    name = "Multivariable Geostatistical Prediction And Simulation",
    short.name = "gstat",
    note = "https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/gstat.
I would recommend using the standard interface for gstat using krige. This combines the building of the gstat object and the prediction into one functions. Very rarely do you need to build gstat objects yourself
https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict"
  )
}

#' @export
trainLearner.regr.gstat = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset)
  #sp::coordinates(d) = ~x+y
  f <- getTaskFormula(.task, explicit.features = TRUE)
  f <- paste(as.character(f)[ c(2,1,3)],collapse="")
  f <- gsub("x + y + ", " ", f, fixed = TRUE) #regex more smart
  browser()
  f = as.formula(f)
  #debug(gstat)
  gstat::gstat(
    formula = f,
    data = d,
    ...
  )
}

#' @export #multivariate prediction https://www.rdocumentation.org/packages/gstat/versions/1.0-2/topics/predict.gstat
predictLearner.regr.gstat = function(.learner, .model, .newdata, ...) {
  #browser()
  if (.learner$predict.type == "response") {
    #predict(.model$learner.model, newdata = .newdata, se.fit = FALSE, ...)
    #d =  getTaskData(.task, .subset)
    #sp::coordinates(d) = ~x+y
    p = predict( # https://www.rdocumentation.org/packages/gstat/versions/1.0-2/topics/predict
      object = .model$learner.model,
      newdata = .newdata
    )
    p = p[[3]]
  } else {
    p = predict(.model$learner.model, newdata = .newdata, se.fit = TRUE, ...)
    cbind(p$fit, p$se.fit)
  }
}

