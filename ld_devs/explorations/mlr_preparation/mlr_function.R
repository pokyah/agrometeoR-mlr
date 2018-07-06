#' Spatialize solar irradiance for an hour and return its result in a dataframe
#' @author Loïc Davadan - ldavadan.github.io
#' @param dfrom.chr A character which specifies the date from which you want to make a benchmark ; respect the format 'YYYY-MM-DD'
#' @param dto.chr A character which specifies the date until which you want to make a benchmark ; respect the format 'YYYY-MM-DD'. Warning ! You will have only the result of midnight for this day
#' @return a dataframe of spatialized solar irradiance for an hour
#' @export
make.benchmark.tasks <- function(static.vars, dynamic.vars, target.chr){
  
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
}



