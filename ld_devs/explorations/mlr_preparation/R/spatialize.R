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


spatialize <- function(learner.cl.chr, learner.id.chr, task, prediction_grid.df, predict.type){
  
  require(mlr)
  
  # create the response learner
  regr.lrn = mlr::makeLearner(
    cl = learner.cl.chr ,
    id = learner.id.chr,
    predict.type = predict.type # could  be "response" or "se"
  )

  # train the resp learner to create the regr model on our dataset
  regr.mod = train(regr.lrn, task)

  # Compute the model response for the target on the grid
  task.pred = predict(
    object = regr.mod,
    newdata = prediction_grid.df
  )
  
  # Group in a spatial sf
  #pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(resp.task.pred), as.data.frame(se.task.pred))
  pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(task.pred))
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
