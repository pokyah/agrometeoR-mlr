#'---
#'author: "Thomas Goossens (CRA-W) - t.goossens@cra.wallonie.be"
#'output: 
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'  md_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true    
#'title: "Collection of R Scripts of the Agromet project"
#'date: \ 20-04-2018\
#'---


spatialize <- function(records.df, task.id.chr, learner.id.chr, learner.cl.chr, target.chr, prediction_grid.df){
  
  require(mlr)
  records.df <- data.frame(records.df)
  
  # creating the regression task
  regr.task = mlr::makeRegrTask(
    id = task.id.chr,
    data = data.frame(records.df$data[c("altitude", "tsa")]),
    target = target.chr,
    coordinates = records.df$data[c("longitude", "latitude")]
  )
  
  # Get some information about the task
  getTaskDesc(regr.task)
  
  # create the response learner
  resp.regr.lrn = mlr::makeLearner(
    cl = learner.cl.chr ,
    id = learner.id.chr,
    predict.type = "response" #could also be "se"
  )

  #train the resp learner to create the regr model on our dataset
  resp.regr.mod = train(resp.regr.lrn, regr.task)
  
  # create the standard error learner
  se.regr.lrn = mlr::makeLearner(
    cl = "regr.lm",
    id = "re.regr.lm",
    predict.type = "se"
  )
  
  # train the se learner to create the model on our dataset
  se.regr.mod = train(se.regr.lrn, regr.task)
  
  # Get infos about the model
  resp.regr.mod$learner
  # print(resp.regr.mod$learner.model)
  # print(summary(resp.regr.mod$learner.model))
  # print(resp.regr.mod$features)
  # print(resp.regr.mod$task.desc$size)
  
  # Compute the model response for the target on the grid
  resp.task.pred = predict(
    object = resp.regr.mod,
    newdata = prediction_grid.df
  )
  
  # Compute the model SE for the target on the grid
  resp.task.pred = predict(
    object = resp.regr.mod,
    newdata = prediction_grid.df
  )

  # Group in a spatial sf
  #pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(resp.task.pred), as.data.frame(se.task.pred))
  pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(resp.task.pred))
  # pred_data.grid.sf <- tsa.model.sf <- st_as_sf(x = pred_data.grid.df, 
  #                                               coords = c("longitude", "latitude"),
  #                                               crs = 4326)
  # 
  # plot <- plot(pred_data.grid.sf)
  # 
  # # Inspect the difference between the true, predicted and SE values
  # print(head(getPredictionResponse(resp.task.pred)))
  # 
  # Return the predicted data and the error
  return(pred_data.grid.df)
}

#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  
