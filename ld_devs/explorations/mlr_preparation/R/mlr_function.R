#' Make tasks for the benchmark
#' @author Loïc Davadan - ldavadan.github.io
#' @param static.vars A simple feature which contains static variables
#' @param dynamic.vars A simple feature which contains records of dynamic variables
#' @param target.chr A character which specifies the parameter you want to predict
#' @param feat_to_drop.chr A character which specifies features you want not to be used like explanatory variables
#' @param filter_method.chr A character to specify the method to use to filter features ; See listFilterMethods
#' @param filter_abs.num A numeric which specifies how many top scoring features you want to select
#' @return a nested dataframe with data and tasks for every hour
#' @export
make.benchmark.tasks <- function(static.vars, dynamic.vars, target.chr, feat_to_drop.chr, filter_method.chr, filter_abs.num){
  
  data.sf <- st_join(static.vars, dynamic.vars)
  
  # Make the sf objects, df again
  data.stations.df <- data.frame(data.sf) %>%
    dplyr::select(-geometry) %>%
    dplyr::select(-gid.y) %>%
    dplyr::select(-gid.x)
  
  # Building a nested data frame, where for each hourly observation we have a 27 stations dataset of 1h record.
  data.stations.n.df <- data.stations.df %>%
    group_by(mtime) %>%
    tidyr::nest()
  
  # converting each tibble of the nested records to a strict dataframe (required by mlr)
  # ::todo:: need to use transmute_at
  data.stations.n.df <- data.stations.n.df %>%
    mutate(data_as_df = purrr::map(
      .x = data,
      .f = data.frame
    ))
  
  # removing the tibbles columns and only keeping the pure dataframes (required by mlr)
  data.stations.n.df <- data.stations.n.df %>%
    dplyr::select(mtime, data_as_df)
  
  # defining the regression tasks on the stations observations for each of the hourly datasets
  # https://stackoverflow.com/questions/46868706/failed-to-use-map2-with-mutate-with-purrr-and-dplyr
  # https://stackoverflow.com/questions/42518156/use-purrrmap-to-apply-multiple-arguments-to-a-function?rq=1
  data.stations.n.df <- data.stations.n.df %>%
    mutate(tasks = purrr::map2(
      as.character(mtime),
      data_as_df,
      mlr::makeRegrTask,
      target = target.chr
    )
    )
  
  # drop unexplanatory features
  data.stations.n.df <- data.stations.n.df %>%
    mutate(filtered_tasks = purrr::map(
      .x = tasks,
      .f = mlr::dropFeatures,
      features = feat_to_drop.chr
      )
    )
  
  # removing the tasks columns and only keeping the filtered tasks
  data.stations.n.df <- data.stations.n.df %>%
    dplyr::select(mtime, data_as_df, tasks, filtered_tasks)
  
  # Define tasks on the 25% best tasks only
  data.stations.n.df$filtered_tasks <-  purrr::map(
    data.stations.n.df$filtered_tasks,
    mlr::filterFeatures,
    method = filter_method.chr,
    abs = filter_abs.num,
    mandatory.feat = "ens"
  )
  
  return(data.stations.n.df)
}

#' Prepare your data to be visualized with its related error on a map
#' @author Loïc Davadan - ldavadan.github.io
#' @param spatialized.tsa_error.sf A simple feature which contains temperature prediction on the grid and related error
#' @param grid.pg.sf A simple feature made of polygons (squares), which forms the grid of Wallonia
#' @param sf.bool A boolean which specifies if you want a sf (TRUE) or a sp (FALSE)
#' @return a spatial object which contains a RGB value for each cell of the grid corresponding to its temperature prediction and its error
#' @export
build.spatialized.tsa_error.pg <- function(spatialized.tsa_error.sf, grid.pg.sf, sf.bool){
  
  ### Prepare spatialized data to be visualize with error on a map
  # Set the visualization values:
  resp.min <- min(spatialized.tsa_error.sf$response, na.rm=TRUE)  # resp.min - lower limit value;
  resp.max <- max(spatialized.tsa_error.sf$response, na.rm=TRUE)  # resp.max - upper limit value;
  se.min <- min(spatialized.tsa_error.sf$se, na.rm=TRUE)  # se.min - lower error value ;
  se.max <- max(spatialized.tsa_error.sf$se, na.rm=TRUE)  # se.max - upper error value ;

  # Strech the values (response) to the inspection range:
  # Mask the values out of the 0-1 range:
  spatialized.tsa_error.sf$norm.response <- (spatialized.tsa_error.sf$response-resp.min)/(resp.max-resp.min)
  spatialized.tsa_error.sf$norm.response.centred <- ifelse(spatialized.tsa_error.sf$norm.response<=0, 0, ifelse(spatialized.tsa_error.sf$norm.response>1, 1, spatialized.tsa_error.sf$norm.response))
  
  # Derive the Hue image:
  # The hues should lie between between 0 and 360, and the saturations
  # and values should lie between 0 and 1.
  spatialized.tsa_error.sf$tmpf1 <- -90-spatialized.tsa_error.sf$norm.response.centred*300
  spatialized.tsa_error.sf$tmpf2 <- ifelse(spatialized.tsa_error.sf$tmpf1<=-360, spatialized.tsa_error.sf$tmpf1+360, spatialized.tsa_error.sf$tmpf1)
  spatialized.tsa_error.sf$H <- ifelse(spatialized.tsa_error.sf$tmpf2>=0, spatialized.tsa_error.sf$tmpf2, (spatialized.tsa_error.sf$tmpf2+360))
  
  # Modify Hue to match with the 'RdYlBu' palette colors from tmap
  # inspired by https://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
  library(data.table)
  H=c(3,14,30,44,192,195,203,214,237,347)
  list_H  <- data.table(H, val = H)
  sp.H <- data.table(spatialized.tsa_error.sf$H)
  setattr(list_H, "sorted", "H")
  Hcorr <- data.frame(list_H[J(sp.H), roll = "nearest"])
  spatialized.tsa_error.sf$H <- Hcorr[,"val"]
  
  # Strech the error values (se) to the inspection range:
  # Mask the values out of the 0-1 range:
  spatialized.tsa_error.sf$norm.se <- (spatialized.tsa_error.sf$se-se.min)/(se.max-se.min)
  spatialized.tsa_error.sf$norm.se.centred <- ifelse(spatialized.tsa_error.sf$norm.se<=0, 0, ifelse(spatialized.tsa_error.sf$norm.se>1, 1, spatialized.tsa_error.sf$norm.se))
  
  # Derive the saturation and intensity images:
  # Modifications to match with 'RdYlBu' palette colors
  spatialized.tsa_error.sf$S <- 1-spatialized.tsa_error.sf$norm.se.centred
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 3] <- 0.82*spatialized.tsa_error.sf$S#0.82-(1-spatialized.tsa_error.sf$S*0.82)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 14] <- 0.73*spatialized.tsa_error.sf$S#0.73-(1-spatialized.tsa_error.sf$S*0.73)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 30] <- 0.78*spatialized.tsa_error.sf$S#0.78-(1-spatialized.tsa_error.sf$S*0.78)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 44] <- 0.43*spatialized.tsa_error.sf$S#0.43-(1-spatialized.tsa_error.sf$S*0.43)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 192] <- 0.10*spatialized.tsa_error.sf$S#0.10-(1-spatialized.tsa_error.sf$S*0.10)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 195] <- 0.27*spatialized.tsa_error.sf$S#0.27-(1-spatialized.tsa_error.sf$S*0.27)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 203] <- 0.43*spatialized.tsa_error.sf$S#0.43-(1-spatialized.tsa_error.sf$S*0.43)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 214] <- 0.62*spatialized.tsa_error.sf$S#0.62-(1-spatialized.tsa_error.sf$S*0.62)
  spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 237] <- 0.67*spatialized.tsa_error.sf$S#0.67-(1-spatialized.tsa_error.sf$S*0.67)
  # spatialized.tsa_error.sf$S[spatialized.tsa_error.sf$H == 347] <- 1*spatialized.tsa_error.sf$S#1-(1-spatialized.tsa_error.sf$S*1)
  
  spatialized.tsa_error.sf$V <- 1
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 3] <- 0.84
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 14] <- 0.96
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 30] <- 0.96
  # spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 44] <- 1
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 192] <- 0.97
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 195] <- 0.91
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 203] <- 0.82
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 214] <- 0.71
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 237] <- 0.58
  spatialized.tsa_error.sf$V[spatialized.tsa_error.sf$H == 347] <- 0.65
  
  # Convert the HSV values to RGB :
  library(colorspace)
  RGBimg <- as(HSV(spatialized.tsa_error.sf$H, spatialized.tsa_error.sf$S, spatialized.tsa_error.sf$V), "RGB")
  summary(RGBimg@coords)
  # put them as R, G, B bands:
  spatialized.tsa_error.sf$red <- RGBimg@coords[,1]
  spatialized.tsa_error.sf$green <- RGBimg@coords[,2]
  spatialized.tsa_error.sf$blue <- RGBimg@coords[,3]
  summary(spatialized.tsa_error.sf[c("red", "green", "blue")])
  spatialized.tsa_error.sf$RGB <- rgb(spatialized.tsa_error.sf$red, spatialized.tsa_error.sf$green, spatialized.tsa_error.sf$blue)
  
  # spatialize each cell according its RGB value
  spatialized.tsa_error.pg.sf <- st_join(grid.pg.sf, spatialized.tsa_error.sf) %>%
    dplyr::select(gid, response, se, norm.response.centred, norm.se.centred, RGB, geometry)
  spatialized.tsa_error.pg.sf$RGB[is.na(spatialized.tsa_error.pg.sf$RGB)] <- "#ffffff"
  
  if(sf.bool == TRUE){
    return(spatialized.tsa_error.pg.sf)
  }else{
    spatialized.tsa_error.pg.sp <- as(spatialized.tsa_error.pg.sf, "Spatial")
    return(spatialized.tsa_error.pg.sp)
  }
}

whiteAlpha <- function() {
  alpha <- seq(0,1,0.1)
  r <- col2rgb("green", alpha=T)
  r <- t(apply(r, 1, rep, length(alpha)))
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
  return(codes)
}

