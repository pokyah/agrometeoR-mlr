# I would recommend using the standard interface for gstat using krige. This combines the building of the gstat object and the prediction into one functions. Very rarely do you need to build gstat objects yourself
# https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict




#' @export
makeRLearner.regr.trendsurface = function(){#https://www.rdocumentation.org/packages/gstat/versions/1.0-2/topics/gstat
  makeRLearnerRegr(
    cl = "regr.trendsurface",
    package = "gstat",
    par.set = makeParamSet(#https://www.rdocumentation.org/packages/ParamHelpers/versions/1.8/topics/LearnerParam
      makeLogicalIntegerParam(id = "degree", default = 2)
    ),
    par.vals = list(degree = 2),
    properties = c("numerics", "factors" , "se", "weights"),
    name = "Trend surfaces",
    short.name = "gstat",
    note = "::TODO::"
  )
}

#' @export
trainLearner.regr.trendsurface = function(.learner, .task, .subset, .weights = NULL,  ...) {
  coordinates(getTaskData(.task, .subset)) = ~x + y
  f = log(getTaskFormula(.task, explicit.features = TRUE))
  d = getTaskData(.task, .subset)
  if (is.null(.weights)) {
    gstat::gstat(formula = f, data = d, ...   ) #https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html
  }
}

#' @export #multivariate prediction https://www.rdocumentation.org/packages/gstat/versions/1.0-2/topics/predict.gstat
predictLearner.regr.trendsurface = function(.learner, .model, .newdata, ...) { #pass extra args like mask here
  if (.learner$predict.type == "response") {
    #predict(.model$learner.model, newdata = .newdata, se.fit = FALSE, ...)
    p = predict( # https://www.rdocumentation.org/packages/gstat/versions/1.0-2/topics/gstat
      object = gstat::gstat(
        formula = getTaskFormula(task, explicit.features = TRUE) ,
        locations= ~x+y,
        data = getTaskData(task),#, subset),
        model = .model$learner.model,
        #beta,
        nmax = Inf, nmin = 0, omax = 0, maxdist = Inf, dummy = FALSE,
        #set,
        fill.all = FALSE, fill.cross = TRUE,
        variance = "identity",
        # weights = weights,
        #merge,
        degree = 0, vdist = FALSE, lambda = 1.0
      ),
      newdata = .newdata)
    p = p[[3]]
    #,
    # block = numeric(0),
    # nsim = 0,
    # indicators = FALSE,
    # BLUE = FALSE,
    # debug.level = 1,
    # na.action = na.pass,
    # sps.args = list(n = 500, type = "regular", offset = c(.5, .5)), ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, se.fit = TRUE, ...)
    cbind(p$fit, p$se.fit)
  }
}

