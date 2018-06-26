
#----
#' Create a color palette based on the default ggplot colours
#'
#' Inspired from https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' @author Thomas Goossens - pokyah.github.io
#' @return a colour palette
#' @export
h.ggplot_colours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

#----
#' Render the desired weather data visualisation plot built with ggplot2
#' @author Thomas Goossens - pokyah.github.io
#' @param records.df A dataframe containing the records you wan to vizualize
#' @param plot.chr a character specifying the type of plot you want to build ("freq", "timeSerie", "scatter")
#' @param sensor_name.chr the name of the sensor data you want to plot
#' @return an object of type plot
#' @export
h.render_plot <- function(records.df, plot.chr, sensor_name.chr){
  # draw the frequency plot
  if(plot.chr=="freq"){
    density.plot <- records.df %>%
      ggplot::ggplot(data=.,
             x = records.df$mtime,
             y = records.df[sensor_name.chr]) +
      aes_string(
        x=sensor_name.chr,
        colour=colnames(records.df)[2]) +
      geom_density() # +
    # ggtitle(paste0(sensor.chr, "(", duration.chr, ") - ", "density"))
    return(density.plot)
  }

  # draw the timeSerie plot
  if(plot.chr=="timeSerie"){
    time.plot <- records.df %>%
      ggplot::ggplot(data=.,
             x = records.df$mtime,
             y = records.df[sensor_name.chr]) +
      aes_string(
        #x=colnames(records.df)[12],
        x="mtime",
        y=sensor_name.chr,
        colour=colnames(records.df)[2]
        # text = paste(
        #   'station :', records.df[3],
        #   '<br>Date: ', as.Date(records.df$mtime),
        #   '<br>Value: ', records.df[sensor_col.num]
        # )
      ) +
      geom_line() #+
    # ggtitle(paste0(sensor.chr, "(", duration.chr, ") - ", "time"))
    return(time.plot)
  }

  # draw the regression plot
  if(plot.chr=="scatter"){
    records.df <- records.df %>% mutate(month = as.factor(month(mtime)))
    # mutate_at(records.df, "mtime", yday)
    # records.df[1] <- apply(records.df[1], 1, yday)
    scatter.plot <- records.df %>%
      ggplot::ggplot(data=.,
             aes_(x= as.name(names(records.df)[2]),
                  y= as.name(names(records.df)[3]),
                  colour= as.name(names(records.df)[4])
             )
      ) +
      labs(x = paste(" station ", colnames(records.df[2]), sep= "")) +
      labs(y = paste(" station ", colnames(records.df[3]), sep= "")) +
      geom_point() +
      geom_smooth(method=lm, color="darkred", fill="blue")
    return(scatter.plot)
  }
}
