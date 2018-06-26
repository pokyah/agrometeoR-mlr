#----
#' Append a boolean column for is it day or not on an input dataframe containing a mtime column
#'
#' @author Thomas Goossens - pokyah.github.io
#' @param records.df a dataframe containing a mtime column
#' @return The same dataframe with a new boolean is it day column
#' @export
h.is_it_day <- function(records.df){

  # declaration of the function to return a boolean for day state
  returnDayState <- function(mtime, sunrise, sunset){
    if(chron::times(strftime(mtime,"%H:%M:%S")) >= sunrise && chron::times(strftime(mtime, format="%H:%M:%S")) <= sunset){
      day <- TRUE
    }else{
      day <- FALSE
    }
    return(day)
  }
  # add a boolean column for day = TRUE or FALSE
  records.df <- records.df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(day=returnDayState(mtime, sunrise, sunset))

  # reorder and return the dataframe
  meta <- subset(records.df, select=(1:12))
  sun <- subset(records.df, select=(c(13,14,length(records.df))))
  sensors <- subset(records.df, select=(15:(length(records.df)-1)))
  records.df <- data.frame(bind_cols(meta, sun, sensors))
  return(records.df)
}


#----
#' Check for NA values in a dataframe and log rows with NA values in a dataframe
#'
#' @author Thomas Goossens - pokyah.github.io
#' @param records.df a dataframe containing the records you want to check for NA's
#' @return a dataframe containing al lthe records with at least one NA value
#' @export
h.check_NA <- function(records.df){
  numberOfNAObs <- nrow(records.df[rowSums(is.na(records.df)) > 0,])
  logNoNa.df <- records.df[rowSums(is.na(records.df))==0,]
  if((numberOfNAObs > 0) ==TRUE){
    cat("NA values found in dataframe. ")
    logNa.df <- records.df[rowSums(is.na(records.df))>0,]
    return(logNa.df)
  }else{
    cat("No NA values found in dataframe. ")
  }
}

#----
#' Transform a linear model list output to a dataframe. Useful for markdown export with [kable](https://www.rdocumentation.org/packages/knitr/versions/1.20/topics/kable)
#'
#' Inspired from http://r.789695.n4.nabble.com/Export-summary-from-regression-output-td4647109.html
#' @author Thomas Goossens - pokyah.github.io
#' @param lm.l an object produced by lm()
#' @return a dataframe presenting the model outputs
#' @export
lm_output_to_df.fun <-function(lm.l){
  values<-c(paste(as.character(summary(lm.l)$call),collapse=" "),
            lm.l$coefficients[1],
            lm.l$coefficients[2],
            length(lm.l$model),
            summary(lm.l)$coefficients[2,2],
            summary(lm.l)$r.squared,
            summary(lm.l)$adj.r.squared,
            summary(lm.l)$fstatistic,
            pf(summary(lm.l)$fstatistic[1],summary(lm.l)$fstatistic[2],summary(lm.l)$fstatistic[3],lower.tail=FALSE))
  names(values)<-c("call","intercept","slope","n","slope.SE","r.squared","Adj. r.squared","F-statistic","numdf","dendf","p.value")
  return(data.frame(values))
}


#----
#' Get the widest shared date range of 2 AWS
#' @author Thomas Goossens - pokyah.github.io
#' @param records.df a dataframe containing the records you want filter
#' @param s1 a character specifying the id of the first station
#' @param s2 a character specifying the id of the second station
#' @param type a character specifying if max date or min date ("min" ou "max)
#' @return a character containing the id of the station
#' @export
h.get_date <- function(records.df, s1, s2, type){
  format = "%Y-%m-%d %H:%M:%S"
  tz = "GMT"
  s1_data <- filter(records.df, sid==get_id(s1))
  s2_data <- filter(records.df, sid==get_id(s2))
  if(type == "min"){
    min_s1 <- as.POSIXct(s1_data$from, format=format, tz=tz)
    min_s2 <- as.POSIXct(s2_data$from, format=format, tz=tz)
    minDate <- max(c(min_s1, min_s2))
    return(minDate)
  }
  if(type == "max"){
    maxDate <- as.POSIXct(Sys.Date())
    if(get_id(s1) == "1000" || get_id(s2) == "1000"){
      maxDate = as.Date("2017-11-01")
    }
    return(maxDate)
  }
}

#----
#' Get sid of sation based on its name_id
#' @author Thomas Goossens - pokyah.github.io
#' @param station_name_id.chr a character specifying the name_sid of the station yo uwant to obtain the id from
#' @return a character containing the id of the station
#' @export
h.get_id <- function(station_name_id.chr){
  id <- strsplit(station_name_id.chr, " - ")[[1]][2]
  return(id)
}

#----
#' Filter a records dataset according to the passed pre-defined filtering parameter
#' @author Thomas Goossens - pokyah.github.io
#' @param records.df A dataframe containing the records you want filter
#' @param sensor.chr a character specifying the name of the sensor for which you want to apply a pre-defined filter
#' @param dateRange.chr a character vector specifying the dateRange on which you want to filter the dataset
#' @param filter.chr a character specifying the pre-defined filter you want to apply to your dataframe
#' @return a filtered dataframe
#' @export
h.filter_records <- function(records.df, sensor.chr, dateRange.chr, filter.chr){
  records.df <- records.df %>%
    dplyr::filter(
      between(as.Date(mtime), dateRange.chr[1], dateRange.chr[2])
    )
  # removing observations with NA values. We need to remove for all stations to keep station by station observations
  station1.df <- records.df %>% dplyr::filter(sid== unique(records.df$sid)[1]) %>% dplyr::filter_(paste("!is.na(",sensor.chr, ")"))
  station2.df <- records.df %>% dplyr::filter(sid== unique(records.df$sid)[2]) %>% dplyr::filter_(paste("!is.na(",sensor.chr, ")"))
  station2.df <- dplyr::semi_join(station2.df, station1.df, by="mtime")
  records.df <- dplyr::bind_rows(station1.df, station2.df)

  # applying the passed filter
  if(filter.chr == "no_extra_filter"){
    records.df
  }
  if(filter.chr == "day_only"){
    records.df <- h.is_it_day(records.df) %>% dplyr::filter(day==TRUE)
  }
  if(filter.chr == "night_only"){
    records.df <- h.is_it_day(records.df) %>% dplyr::filter(day==FALSE)
    h.check_NA(records.df)
  }
  if(filter.chr == "high_rad_low_wind"){
    vvt.q75.df <- records.df %>%
      dplyr::summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
    day.records.df <- dplyr::filter(h.is_it_day(records.df), day==TRUE)
    ens.day.mean.df <- day.records.df %>%
      dplyr::summarise_(.dots = paste0('mean(', "ens",', na.rm = TRUE)'))
    retained.df <- records.df %>% dplyr::filter(vvt < vvt.q75.df[1,]) %>% dplyr::filter(ens >= ens.day.mean.df[1,])
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  #### right part of BA - hypothesis 1
  if(filter.chr == "up10deg"){
    retained.df <- records.df %>% dplyr::filter(tsa > 10)
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  if(filter.chr == "low_rad_high_wind"){
    vvt.q50.df <- records.df %>%
      dplyr::summarise_(.dots = paste0('quantile(', "vvt",', probs= .50, na.rm = TRUE)'))
    retained.df <- records.df %>% dplyr::filter(ens == 0) %>% dplyr::filter(vvt >= vvt.q50.df[1,])
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  if(filter.chr == "low_rad_high_wind_up10"){
    vvt.q75.df <- records.df %>%
      dplyr::summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
    retained.df <- records.df %>% dplyr::filter(vvt >= vvt.q75.df[1,]) %>% dplyr::filter(ens == 0) %>% dplyr::filter(tsa > 10)
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  #### left part of BA - hypothesis 2
  if(filter.chr == "below10deg"){
    retained.df <- records.df %>% dplyr::filter(tsa <= 10)
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  if(filter.chr == "high_rad_high_wind"){
    vvt.q75.df <- records.df %>%
      dplyr::summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
    day.records.df <- dplyr::filter(h.is_it_day(records.df), day==TRUE)
    ens.day.mean.df <- day.records.df %>%
      dplyr::summarise_(.dots = paste0('mean(', "ens",', na.rm = TRUE)'))
    retained.df <- records.df %>% dplyr::filter(vvt >= vvt.q75.df[1,]) %>% dplyr::filter(ens >= ens.day.mean.df[1,])
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  if(filter.chr == "high_rad_high_wind_below10"){
    vvt.q75.df <- records.df %>%
      dplyr::summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
    day.records.df <- dplyr:filter(h.is_it_day(records.df), day==TRUE)
    ens.day.mean.df <- day.records.df %>%
      dplyr::summarise_(.dots = paste0('mean(', "ens",', na.rm = TRUE)'))
    retained.df <- records.df %>% dplyr::filter(vvt >= vvt.q75.df[1,]) %>% dplyr::filter(ens >= ens.day.mean.df[1,]) %>% dplyr::filter(tsa <= 10)
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  #### clearness index
  if(filter.chr == "q70_ci"){
    ci.q70.df <- records.df %>%
      dplyr::filter(ci > 0) %>%
      dplyr::summarise_(.dots = paste0('quantile(', "ci",', probs= .70, na.rm = TRUE)'))
    retained.df <- records.df %>% dplyr::filter(ci >= ci.q70.df[1,])
    records.df <- records.df %>%
      dplyr::filter(mtime %in% retained.df$mtime)
  }
  #### daily_max +/- 2h
  if(filter.chr == "daily_max_only"){
    pameseb61.df <- records.df %>%
      dplyr::filter(sid==61)
    pameseb61.df$key <- as.numeric(row.names(pameseb61.df))
    irm1000.df <- records.df %>%
      dplyr::filter(sid==1000)
    irm1000.df$key <- as.numeric(row.names(irm1000.df))

    daily_max_inds.df <- pameseb61.df %>%
      dplyr::filter(tsa == daily_max) %>%
      dplyr::select(key)
    daily_max_mp2_inds.df <- dplyr::bind_rows(
      daily_max_inds.df +1,
      daily_max_inds.df -1,
      daily_max_inds.df +2,
      daily_max_inds.df -2,
      daily_max_inds.df +3,
      daily_max_inds.df -3,
      daily_max_inds.df) %>%
      dplyr::mutate(key=sort(key))

    daily_max_mp2_pameseb61.df <- pameseb61.df[daily_max_mp2_inds.df[,1],]
    daily_max_mp2_irm1000.df <- irm1000.df[daily_max_mp2_inds.df[,1],]

    records.df <- data.frame(dplyr:bind_rows(
      dplyr::select(daily_max_mp2_pameseb61.df, -key),
      dplyr::select(daily_max_mp2_irm1000.df, -key)
    ))

    # hack to avoid duplicate rows
    records.df <- unique(records.df)
  }
  #### daily_max +/- 2h
  if(filter.chr == "non_daily_max_only"){
    pameseb61.df <- records.df %>%
      dplyr::filter(sid==61)
    pameseb61.df$key <- as.numeric(row.names(pameseb61.df))
    irm1000.df <- records.df %>%
      dplyr::filter(sid==1000)
    irm1000.df$key <- as.numeric(row.names(irm1000.df))

    daily_max_inds.df <- pameseb61.df %>%
      dplyr::filter(tsa == daily_max) %>%
      dplyr::select(key)
    daily_max_mp2_inds.df <- dplyr::bind_rows(
      daily_max_inds.df +1,
      daily_max_inds.df -1,
      daily_max_inds.df +2,
      daily_max_inds.df -2,
      daily_max_inds.df +3,
      daily_max_inds.df -3,
      daily_max_inds.df) %>%
      dplyr::mutate(key=sort(key))

    daily_max_mp2_pameseb61.df <- pameseb61.df[-(daily_max_mp2_inds.df[,1]),]
    daily_max_mp2_irm1000.df <- irm1000.df[-(daily_max_mp2_inds.df[,1]),]

    records.df <- data.frame(dplyr:bind_rows(
      daily_max_mp2_pameseb61.df,
      daily_max_mp2_irm1000.df
    ))
    # hack to avoid duplicate rows
    records.df <- unique(records.df)
  }
  return(records.df)
}


#----
#' Transform a records dataframe from long to wide format
#' @author Thomas Goossens - pokyah.github.io
#' @param input_records.df A dataframe containing the records you want to reshape in wide format
#' @param sensor_name.chr a character specifying the name of the sensor you want to reshape in wide format
#' @return a wide dataframe containing the records for the specified sensor
#' @export
h.make_wide <- function(input_records.df, sensor_name.chr){
  input_records_wide.df <- input_records.df[c("mtime", "sid", sensor_name.chr)] %>%
    tidyr::spread_("sid", sensor_name.chr)
  return(na.omit(input_records_wide.df))
}

#----
#' Compute the summary stats for the desired sensor
#' @author Thomas Goossens - pokyah.github.io
#' @param input_records.df A dataframe containing the records on which you want to compute the summary stats for a sensor
#' @param sensor_name.chr a character specifying the name of the sensor on which you want to compute the summary stats
#' @return a dataframe containing the summary stats of the specified sensor
#' @export
h.compute_stats <- function(input_records.df, sensor_name.chr){

  funs <- c("sum", "mean", "min", "max", "sd", "var")
  stats <- paste0("round(", funs, "(", sensor_name.chr,", na.rm=TRUE), 2)")
  names(stats) <- funs
  as.list(stats)

  probs <- c("25", "50", "75")
  quants <- paste0("round(quantile(", sensor_name.chr, ",probs=.", probs, ", na.rm=TRUE), 2)")
  names(quants) <- paste0("q", probs)
  as.list(quants)

  summarys_stats <- do.call(c, list(stats, quants))

  # Compute summary
  summary.df <- input_records.df %>% select_("id", "sid", "poste", sensor_name.chr) %>%
    group_by_("id", "sid", "poste") %>%
    #group_by(.dots=names(input_records.df)[-grep(sensor_name.chr, names(input_records.df))]) %>%
    #group_by_at_(vars(-sensor_name.chr)) %>%
    summarise_(.dots = summarys_stats)

  # return summary
  return(summary.df)
}

#----
#' Compute a simple linear model and return its statistics in a dataframe
#' @author Thomas Goossens - pokyah.github.io
#' @param records.wide.df A wide dataframe containing the records you wan to submit to lm
#' @param output a character specifying the type of output you want ("lm.sm" or "lm")
#' @return a dataframe, either from lm or lm.sum
#' @export
compute_lm <- function(records.wide.df, output){
  station1 <- records.wide.df[2]
  station2 <- records.wide.df[3]
  lm.mod <- lm(as_vector(station1)~as_vector(station2))
  lm.mod.sum <- summary(lm.mod)
  lm.mod.sum.df <- broom::tidy(lm.mod.sum)
  lm.mod.df <- broom::glance(lm.mod)

  if(output=="lm.sum"){
    return(lm.mod.sum.df)
  }
  if(output=="lm"){
    return(lm.mod.df)
  }
}

#----
#' Compute either a Bland-Altman plot or Bland-Altman stats dataframe
#' @author Thomas Goossens - pokyah.github.io
#' @param records.wide.df A wide dataframe containing the records you wan to submit to BA analysis
#' @param output.chr a character specifying the type of output you want ("plot" or "table")
#' @param sensor_name.chr the name of the sensor data you want to plot
#' @return either a BA plot or table
#' @export
h.compute_ba <- function(records.wide.df, output.chr){
  records.wide.df <- records.wide.df %>% dplyr::mutate(month = as.factor(month(mtime)))
  station1 <- records.wide.df[2] # irm1000
  station2 <- records.wide.df[3] # pameseb61

  # compute the stats
  ba_stats.l <- bland.altman.stats(station1[[1]], station2[[1]])
  ba_data.df <- dplyr::bind_cols(as.data.frame(ba_stats.l$means), as.data.frame(ba_stats.l$diffs))
  ba_data.df <- dplyr::bind_cols(ba_data.df, records.wide.df["mtime"], records.wide.df["month"] )
  colnames(ba_data.df) <- c("means", "diffs", "mtime", "month")

  # build the ba plot
  blandAltman_plot.l <- ggplot::ggplot(ba_data.df, aes(x=means, y=diffs, color=month)) +
    geom_point() +
    geom_smooth(method=glm, se=TRUE, color="black", linetype="dashed") +
    geom_hline(yintercept= 0, color = "black", size=0.5) +
    geom_hline(yintercept= ba_stats.l$lines[2], color = "red", size=0.5) +
    geom_hline(yintercept= ba_stats.l$lines[1], color = "blue", size=0.5) +
    geom_hline(yintercept= ba_stats.l$lines[3], color = "blue", size=0.5) +
    geom_hline(yintercept= ba_stats.l$CI.lines[1], linetype="dashed", color = "blue", size=0.5) +
    geom_hline(yintercept= ba_stats.l$CI.lines[2], linetype="dashed", color = "blue", size=0.5) +
    geom_hline(yintercept= ba_stats.l$CI.lines[5], linetype="dashed", color = "blue", size=0.5) +
    geom_hline(yintercept= ba_stats.l$CI.lines[6], linetype="dashed", color = "blue", size=0.5) +
    geom_hline(yintercept= ba_stats.l$CI.lines[3], linetype="dashed", color = "red", size=0.5) +
    geom_hline(yintercept= ba_stats.l$CI.lines[4], linetype="dashed", color = "red", size=0.5) +
    scale_color_manual(values= h.ggplot_colours(n=12))

  if(output.chr=="plot"){
    return(blandAltman_plot.l)
  }
  if(output.chr=="table"){
    ba_stats.l <- as.data.frame(ba_stats.l[4:length(ba_stats.l)])
    ba_stats.l[-1,c(1,2,3,4,7,8)] <- NA
    return(ba_stats.l)
  }
  if(output.chr=="data"){
    return(ba_data.df)
  }
}

#----
#' Compute the value of Clearness index on the basis of mtime, lon and lat and append it as a new column
#' @author Thomas Goossens - pokyah.github.io & Michel Journée (RMI)
#' @param records.df A dataframe containing the records from the stations
#' @return The dataframe with a newly computed CI column
#' @export
h.rad_top_atm <- function(records.df){

  compute_rad_top_atm=function(datetime.dt, lat.num, lon.num){

    RAD2DEG=180./pi
    DEG2RAD=pi/180.

    sunrise_sunset=function(lat, lon, DAYCODE){
      lambda=lon*DEG2RAD
      phi=lat*DEG2RAD
      year=floor(as.numeric(DAYCODE)/10000)

      #  DAILY SOLAR PARAMETERS
      julian_day = make_julian_day(DAYCODE)
      day_angle = Day_Angle(julian_day)
      delta = declination_sun(year, julian_day, lambda)
      time_diff = time_difference_between_LAT_UT(julian_day, lambda)

      #  SUNRISE + SUNSET HOURS (decimal hours in UTC)
      res = sunrise_hour_angle(phi, delta, -1)
      omega_sr=res[1]
      omega_ss=res[2]
      hour_sr=omega_to_LAT(omega_sr)
      hour_ss=omega_to_LAT(omega_ss)
      hour_sr=hour_sr-time_diff
      hour_ss=hour_ss-time_diff
      return(c(hour_sr, hour_ss))
    }

    daylength=function(lat, lon, DAYCODE){
      res=sunrise_sunset(lat, lon, DAYCODE)
      return(res[2]-res[1])
    }

    solar_angles=function(lat, lon, DAYCODE, hour){
      year=floor(as.numeric(DAYCODE)/10000)
      lambda=lon*DEG2RAD
      phi=lat*DEG2RAD

      #  DAILY SOLAR PARAMETERS
      julian_day = make_julian_day(DAYCODE)
      day_angle = Day_Angle(julian_day)
      delta = declination_sun(year, julian_day, lambda)
      time_diff = time_difference_between_LAT_UT(julian_day, lambda)

      #  SOLAR ELEVATION, ZENITHAL ANGLE and AZIMUTHAL ANGLE  (in radian)
      omega = solar_hour_angle(hour+time_diff)
      res = elevation_zenith_sun(phi, delta, omega)
      gamma=res[1]
      theta=res[2]
      alpha = azimuth_sun(phi, delta, omega, gamma)
      return(c(gamma, theta, alpha))
    }

    E_solar_radiation=function(lat, lon, DAYCODE, hour1, hour2){ #decimal hour in UTC
      lambda=lon*DEG2RAD
      phi=lat*DEG2RAD
      year=floor(as.numeric(DAYCODE)/10000)

      #  DAILY SOLAR PARAMETERS
      julian_day = make_julian_day(DAYCODE)
      day_angle = Day_Angle(julian_day)
      delta = declination_sun(year, julian_day, lambda)
      time_diff = time_difference_between_LAT_UT(julian_day, lambda)
      eccentricity=corr_distance(day_angle)

      #  TOP-OF-ATMOSPHERE HORIZONTAL SOLAR RADIATION BETWEEN 2 TIMESTAMPS (in Wh/m²)
      omega1 = solar_hour_angle(hour1+time_diff)
      omega2 = solar_hour_angle(hour2+time_diff)
      E = G0_general(phi, eccentricity, delta, omega1, omega2)
      return(E)
    }

    make_julian_day=function(DAYCODE){
      #  The procedure "make_julian_day" converts a day given in day, month and year into a julian day.
      #  Outputs :  julian_day : integer day number or julian day (1..366)

      tmp = as.Date(DAYCODE, format = "%Y%m%d")
      julian_day=as.numeric(format(tmp, "%j"))
      return(julian_day)
    }

    Day_Angle=function(julian_day){
      #  Inputs :   julian_day : integer day number or julian day (1..366)
      #  Outputs :  day_angle : day angle (in radians)
      #  The procedure "Day_Angle" expresses the integer day number as an angle (in radians) from 12:00 hours on the day 31st December. A year length of 365.2422 days is used.

      day_angle = julian_day * 2.0 * pi / 365.2422
      return(day_angle)
    }

    declination_sun=function(year_number, julian_day, lambda){
      #    Sources :
      #    Bourges, B., 1985. Improvement in solar declination computation. Solar
      #    Energy, 35 (4), 367-369.
      #    Carvalho, M.J. and Bourges, B., 1986. Program Eufrad 2.0 - User's Guide.
      #    Project EUFRAT final scientific report, Contract EN3S-0111-F, Solar Energy
      #    and Development in the European Community, pp. 12.1-12.74.
      #    Duffie, J.A. and Beckman, W.A., 1980. Solar Engineering of Thermal
      #    Processes. Wiley-Interscience, New York.
      #    Inputs :
      #    year_number : year number (4 digits)
      #    julian_day  : integer day number or julian day (1..366)
      #    lambda      : longitude (in radians, positive to East)
      #    Outputs :
      #    delta : solar declination angle at noon (in radians)
      #    The procedure "declination_sun" computes the solar declination at noon in
      #    solar time (in radians). A single (average) value per day -at noon- is
      #    adequate for pratical calculations. The noon declination depends on
      #    longitude, as noon occurs earlier if longitude is East of Greenwich, and
      #    later if it is West. The chosen algorithm uses 1957 as base year; it is
      #    basically a truncated Fourier series with six harmonics.

      wt = 0.
      b1 =  0.0064979
      b2 =  0.4059059
      b3 =  0.0020054
      b4 = -0.0029880
      b5 = -0.0132296
      b6 =  0.0063809
      b7 =  0.0003508

      #    n0 : spring-equinox time expressed in days from the beginning of the year
      #     i.e. the time in decimal days elapsing from 00:00 hours Jan 1st to the
      #     spring equinox at Greenwich in a given year
      #   t1 : time in days, from the spring equinox 0.5 represents the decimal day number at noon on Jan 1st at Greenwich
      n0 = 78.8946 + 0.2422*(year_number-1957) - 0.25*(year_number-1957)
      t1 = - 0.5 - lambda / (2 * pi) - n0
      w0 = 2 * pi / 365.2422
      wt = w0 * (julian_day + t1)
      delta = b1 + b2 * sin(wt) + b3 * sin(2 * wt) + b4 * sin(3 * wt) + b5 * cos(wt) + b6 * cos(2 * wt) + b7 * cos(3 * wt)
      return(delta)
    }

    geogr_to_geoce=function(phi_g){
      CC=0.99330552        # Correction factor for converting geographic
      # into geocentric latitude. CC=(Rpole/Requator)**2
      # Rpole=6356.752, Requator=6378.137
      if((phi_g >= -(pi/2.0-0.0002)) | (phi_g <= (pi/2.0-0.0002))){
        phi=atan(tan(phi_g)*CC)
      } else {
        phi=phi_g
      }
      return(phi)
    }

    sunrise_hour_angle=function(phi_g, delta, gamma_riset){
      #  Source :
      #  Inputs :
      #    phi_g       : latitude of site (in radians, positive to North)
      #    delta       : solar declination angle (in radians)
      #    gamma_riset : solar elevation near sunrise/sunset:
      #                  - set to  0.0 for astronomical sunrise/sunset
      #          - set to -1.0 for refraction corrected sunrise/sunset.
      #  Outputs :
      #    omega_sr : sunrise solar hour angle (in radians)
      #    omega_ss : sunset solar hour angle (in radians)
      #  The procedure "sunrise_hour_angle" supplies the sunrise and sunset hour
      #    angles (in radians). Due to the dimension of the solar disk and the effect
      #    of the atmospheric refraction, the edge of the solar disk will just appear
      #    (disappear) at the horizon at sunrise (at sunset) when the calculated
      #    astronomical elevation is 50'.

      cos_omegas = 0.
      omegas = 0.

      horizon = (-50.0 / 60.0) * DEG2RAD  # horizon, -50' in radians
      if(gamma_riset >= horizon){
        horizon = gamma_riset
      }

      phi=geogr_to_geoce(phi_g)
      max_delta = 23.45 * DEG2RAD
      if ( (abs(phi) < (pi/2.0)) & (abs(delta) <= max_delta)){
        cos_omegas = (sin(horizon) - (sin(phi) * sin(delta))) / (cos(phi) * cos(delta))
      }

      if(abs(cos_omegas) < 1.0){
        omegas = acos(cos_omegas)
      }
      if(cos_omegas >= 1.0){  # the sun is always below the horizon : polar night
        omegas = 0.0
      }
      if(cos_omegas <= -1.0) {# the sun is always above the horizon : polar day
        omegas = pi
      }

      omega_sr = -omegas
      omega_ss =  omegas
      return(c(omega_sr, omega_ss))
    }

    time_difference_between_LAT_UT=function(julian_day, lambda) {
      ier = 1
      a1 = -0.128
      a2 = -0.165
      a3 =  2.80 * DEG2RAD
      a4 = 19.70 * DEG2RAD

      day_angle = Day_Angle(julian_day)
      ET = a1 * sin(day_angle - a3) + a2 * sin(2.0 * day_angle + a4) # in decimal hours
      time_diff=ET + (lambda * 12.0 / pi) # in decimal hour
      #LAT = UT + time_diff
      return(time_diff)
    }

    omega_to_LAT=function(omega){
      #  Source :
      #  Inputs :
      #    omega : solar hour angle (in radians)
      #  Outputs :
      #    t : solar time i.e. LAT (0..24 decimal hours)
      #  The procedure "omega_to_LAT" does the reverse operation of the procedure
      #    "solar_hour_angle" i.e. computes the solar time (in decimal hours) from the
      #    solar hour angle (in radians).

      t = 12.0 * (1.0 + omega / pi)
      return(t)
    }

    solar_hour_angle=function(t){
      #  Source :
      #  Inputs :
      #    t : solar time i.e. LAT (0..24 decimal hours)
      #  Outputs :
      #    omega : solar hour angle (in radians)
      #  The procedure "solar_hour_angle" supplies the solar hour angle (in radians).
      #    By convention the hour angle is negative before noon and positive after noon
      omega = (t - 12.0) * pi / 12.0
      return(omega)
    }

    elevation_zenith_sun=function(phi_g, delta, omega){
      #  Source :
      #  Inputs :
      #    phi_g : latitude of site (in radians, positive to North)
      #    delta : solar declination angle (in radians)
      #    omega : solar hour angle (in radians)
      #  Outputs :
      #    gamma : solar altitude angle (in radians)
      #    theta : solar zenithal angle (in radians)
      #  The procedure "elevation_zenith_sun" computes the solar elevation (or
      #    altitude) angle and the solar zenithal (or incidence) angle. These two
      #    angles are complementary.

      phi=geogr_to_geoce(phi_g)
      res = sunrise_hour_angle(phi_g,delta,0.0)
      omega_sr=res[1]
      omega_ss=res[2]
      if((omega < omega_sr) || (omega > omega_ss)){
        gamma = 0.0
      } else {
        gamma = asin( sin(phi) * sin(delta) + cos(phi) * cos(delta) * cos(omega) )
      }
      if (gamma < 0.0){
        gamma = 0.0
      }
      theta = (pi / 2.0) - gamma
      return(c(gamma, theta))
    }

    azimuth_sun=function(phi_g, delta, omega, gamma){
      #  Source :
      #  Inputs :
      #    phi_g : latitude of site (in radians, positive to North)
      #    delta : solar declination angle (in radians)
      #    omega : solar hour angle (in radians)
      #    gamma : solar altitude angle (in radians)
      #  Outputs :
      #    alpha : solar azimuthal angle (in radians)
      #  The procedure "azimuth_sun" computes the solar azimuth angle in the Northern
      #    hemisphere. The azimuth angle has a positive value when the sun is to the
      #    west of South, i.e. during the afternoon in solar time. For the Southern
      #    hemisphere, the azimuth angle is measured from North.

      phi=geogr_to_geoce(phi_g)
      cos_as = (sin(phi) * sin(gamma) - sin(delta)) / (cos(phi) * cos(gamma))
      if(phi < 0.0) cos_as = -cos_as  #  Southern hemisphere
      sin_as = cos(delta) * sin(omega) / cos(gamma)
      x = acos(cos_as)
      if(sin_as >= 0.0){
        alpha =  x
      } else {
        alpha = -x
      }
      return(alpha)
    }

    corr_distance=function(day_angle){
      #  Source : Gruter (ed.) (1984)
      #  Inputs :
      #    day_angle : day angle (in radians)
      #  Outputs :
      #    eccentricity : correction for Earth orbit eccentricity
      #  The procedure "corr_distance" computes the correction for the variation of
      #    sun-earth distance from its mean value (also known as eccentricity). It is a
      #    fucntion of time, but a single (average) value per day is enough for
      #    practical calculations.

      a = 2.80 * DEG2RAD
      eccentricity = 1.0 + 0.03344 * cos(day_angle - a)
      return(eccentricity)
    }

    G0_general=function(phi_g, eccentricity, delta, omega1, omega2){
      #  Source :
      #  Inputs :
      #    phi_g        : latitude of site (in radians, positive to North)
      #    eccentricity : correction for Earth orbit eccentricity
      #    delta        : solar declination angle (in radians)
      #    omega1       : solar hour angle at beginning of the period (in radians)
      #    omega2       : solar hour angle at end of the period (in radians)
      #  Outputs :
      #    G0_12 : extraterrestrial solar irradiation (in Wh/m2)
      #  The procedure "G0_general" delivers the extraterrestrial solar irradiation
      #    incident on an horizontal surface in the general case (in Wh/m2).

      I0=1367.0 # solar constant in W/m2
      Dl=24.0  #average value for the length of the day in decimal hours

      phi=geogr_to_geoce(phi_g)
      res = sunrise_hour_angle(phi_g,delta,0.0)
      omega_sr=res[1]
      omega_ss=res[2]
      if(omega1 < omega_sr){
        omega1 = omega_sr
      }
      if(omega2 < omega_sr){
        omega2 = omega_sr
      }
      if(omega1 > omega_ss){
        omega1 = omega_ss
      }
      if(omega2 > omega_ss){
        omega2 = omega_ss
      }

      if(omega2 <= omega1){
        G0_12 = 0.0
      } else {
        a = I0 * eccentricity * Dl / (2.0 * pi)
        b1 = sin(phi) * sin(delta) * (omega2 - omega1)
        b2 = cos(phi) * cos(delta) * (sin(omega2) - sin(omega1))
        c = a * (b1 + b2)
        if(c < 0.0){
          G0_12 = 0.0
        } else {
          G0_12 = c
        }
      }
      return(G0_12)
    }

    #  DEFINE DAY AND LOCATION
    DAYCODE = strftime( records.df$mtime[17], format="%Y%m%d")
    #gsub('-', '', as.character(datetime.dt))
    #strftime(datetime.dt, format="%Y%m%d%h%m%s")
    lat=lat.num
    lon=lon.num

    year= floor(as.numeric(DAYCODE)/10000) #lubridate::year(datetime.dt)
    lambda=lon*DEG2RAD
    phi=lat*DEG2RAD

    #  DAILY SOLAR PARAMETERS
    julian_day = make_julian_day(DAYCODE)
    day_angle = Day_Angle(julian_day)
    delta = declination_sun(year, julian_day, lambda)
    time_diff = time_difference_between_LAT_UT(julian_day, lambda)

    #  SUNRISE + SUNSET HOURS (decimal hours in UTC)
    res = sunrise_hour_angle(phi, delta, -1)
    omega_sr=res[1]
    omega_ss=res[2]
    hour_sr=omega_to_LAT(omega_sr)
    hour_ss=omega_to_LAT(omega_ss)
    hour_sr=hour_sr-time_diff
    hour_ss=hour_ss-time_diff

    #  SOLAR ELEVATION, ZENITHAL ANGLE and AZIMUTHAL ANGLE  (in radian)
    hour= as.numeric(strftime(datetime.dt, format="%H")) # 12.5;  #decimal hour in UTC
    omega = solar_hour_angle(hour+time_diff)
    res = elevation_zenith_sun(phi, delta, omega)
    gamma=res[1]
    theta=res[2]
    alpha = azimuth_sun(phi, delta, omega, gamma)

    #  TOP-OF-ATMOSPHERE HORIZONTAL SOLAR RADIATION BETWEEN 2 TIMESTAMPS (in Wh/m²)
    hour1= as.numeric(strftime(datetime.dt, format="%H")) #12.5  #decimal hour in UTC
    hour2= as.numeric(strftime(datetime.dt, format="%H"))+1 # 14.5  #decimal hour in UTC
    omega1 = solar_hour_angle(hour1+time_diff)
    omega2 = solar_hour_angle(hour2+time_diff)
    eccentricity=corr_distance(day_angle)
    E = G0_general(phi, eccentricity, delta, omega1, omega2)
    return(E)
  }

  # add a column rad_top_atm
  records.df <- records.df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rad_top_atm=compute_rad_top_atm(datetime.dt=mtime, lat.num=latitude, lon.num=longitude))

  # add a column with the clearness index
  records.df <- records.df %>%
    dplyr::mutate(ci=ens/rad_top_atm) %>%
    dplyr::mutate_at(.vars = vars(ci), .funs = funs(ifelse(is.na(.), 0, .))) %>% #replacing NA by 0
    dplyr::mutate_at(.vars = vars(ci), .funs = funs(ifelse(.>1, 1, .))) #replacing values larger than 1 by 1

  # return the dataframe containing the rad_top_atm and ci columns
  return(records.df)
}
