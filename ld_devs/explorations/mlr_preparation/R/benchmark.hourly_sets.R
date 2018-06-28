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


benchmark.hourly_sets <- function(nested.records.df, target.chr){
  
  require(mlr)
  
  # defining the target var
  target.chr = "tsa"
  
  # defining the validation (resampling) strategy
  resampling.l = mlr::makeResampleDesc(
    method = "LOO"#,
    #predict = "test"
  )
  

  # # converting each tibble of the nested records to a strict dataframe
  # # ::todo:: need to use transmute_at
  # nested.records.df <- nested.records.df %>%
  #   mutate(data_as_df = purrr::map(
  #     .x = data,
  #     .f = data.frame
  #     ))
  
  # defining the regression tasks for each of the hourly datasets
  # https://stackoverflow.com/questions/46868706/failed-to-use-map2-with-mutate-with-purrr-and-dplyr
  #https://stackoverflow.com/questions/42518156/use-purrrmap-to-apply-multiple-arguments-to-a-function?rq=1
  nested.records.df <- nested.records.df %>%
    mutate(task = purrr::map2(
      as.character(mtime),
      data_as_df,
      mlr::makeRegrTask,
      target = target.chr
    )
    )

  # keeping only the useful features (vars)
  # u.nested.records.df <- nested.records.df %>%
  #   mutate(data_u = purrr::map(
  #     .x = data,
  #     .f = dplyr::select_(
  #       one_of(c("longitude", "latitude", "altitude", "tsa"))
  #       )
  #     ))
  
  # defining the list of tasks from the nested records
  tasks.l <- nested.records.df$task
  
  # defining the learners who will be compared
  lrns.l <- list(
    makeFilterWrapper(
      learner = makeLearner(cl = "regr.lm", id="linear regression"),  fw.method = "information.gain", fw.abs = 2),
    # makeLearner(cl = "regr.lm", id="linear regression"),
    # makeLearner(cl = "regr.elmNN", id="single layer neural net"),
    # makeLearner(cl ="regr.kknn", id="nearest neighbours"),
    makeLearner(cl = "regr.km", id="kriging")
  )
  
  bmr.l <- benchmark(learners = lrns.l, tasks = tasks.l, resamplings = resampling.l, keep.pred = TRUE, show.info = TRUE)
  
  return(bmr.l)
  
  # https://mlr-org.github.io/mlr/articles/tutorial/devel/nested_resampling.html
  # https://mlr-org.github.io/mlr/articles/tutorial/devel/feature_selection.html
  
  
 # # getting the predictions from the model
 #  stations.pred.l <- getBMRPredictions(benchmark.l)
 #  
 #  # predicting on the grid 
 #  meuse.grid.pred <- predict(
 #    train(lrns.l[[1]], spatialization.task),
 #    newdata = meuse.grid.df
 #  )
 #  
 #  meuse.grid.pred.data <- dplyr::bind_cols(meuse.grid.df, meuse.grid.pred$data )
 #  coordinates(meuse.grid.pred.data) <- ~x+y
 #  class(meuse.grid.pred.data)
 #  
 #  spplot(meuse.grid.pred.data)
 #  
 #  
 #  
 #  spplot(meuse$zinc)
 #  
 #  
 #  # Group in a spatial sf
 #  #pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(resp.task.pred), as.data.frame(se.task.pred))
 #  pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(se.task.pred))
 #  pred_data.grid.sf <- tsa.model.sf <- st_as_sf(x = pred_data.grid.df, 
 #                                                coords = c("longitude", "latitude"),
 #                                                crs = 4326)
 #  
 #  plot <- plot(pred_data.grid.sf)
 #  
 #  # Inspect the difference between the true, predicted and SE values
 #  print(head(getPredictionResponse(resp.task.pred)))
 #  
 #  # Return the predicted data and the error
 #  return(plot)
  
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
