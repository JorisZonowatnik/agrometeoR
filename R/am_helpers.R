sids = c("61", "1")
dfrom = "2015-11-01"
dto = "2017-11-01"
sensors = c("tsa", "ens", "vvt")

makeComparison = function(sids, dfrom, dto){

  # creating the dataset
  dataset = agrometeoR::makeDataset(
    stations = sids,
    dfrom = dfrom,
    dto = dto,
    sensor = c(sensors, "sunrise", "sunset"))
  datasetDf = do.call(rbind, dataset$output$value)

  # removing potential NA values and raise warning
  logNa = datasetDf[rowSums(is.na(datasetDf)) > 0,]
  datasetDfNoNa = datasetDf[rowSums(is.na(datasetDf)) == 0,]
  if (!is.null(nrow(logNa))) {
    warning("Your dataset contains NA values that will be removed from the comparison.
      These values were logged and are accessible")
  }

  # keeping only the commonly shared mtime by the 2 stations
  datasetDfclean = subset(datasetDf, !as.character(mtime) %in% as.character(logNa$mtime))

  # adding day or night
  datasetDfDay = data.frame(
    datasetDfclean %>%
      rowwise() %>% mutate(day = am_returnDayState(mtime, sunrise, sunset)))


  # datasetDfDayRad = am_getTopAtmosRad(datasetDfDay, mtime = "mtime", longitude = "y", latitude = "x")

  # Add a date column
  datasetDfDayDmax <- datasetDfDay %>%
    mutate(date = date(mtime))

  # add a dailyMax column
  datasetDfDayDmax <- data.frame(
    datasetDfDayDmax %>%
      dplyr::group_by(sid) %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(daily_max = max(tsa)) %>%
      dplyr::ungroup())

  # add a month column for the plots
  datasetDfDayDmax = datasetDfDayDmax

  # Compute the summary stats for each sensor
  output_stats = lapply(sensors, function(x){
    am_makeStats(datasetDfDayDmax, x)
  })
  names(output_stats) = sensors

  # compute the blandAltman
  output_blandAltman = am_makeBlandAltman(
    records = datasetDfDayDmax , sids = sids )

  # compute the classic plots
  output_plots = lapply(sensors, function(x){
    am_makePlots(datasetDfDayDmax, x, sids)
  })
  names(output_plots) = sensors


}


# declaration of the function to return a boolean for day state
am_returnDayState = function(mtime, sunrise, sunset){
  if (times(strftime(mtime,"%H:%M:%S")) >= sunrise && times(strftime(mtime, format = "%H:%M:%S")) <= sunset){
    day = TRUE
  } else{
    day = FALSE
  }
  return(day)
}

am_makeStats = function(records, sensor){

  funs = c("sum", "mean", "min", "max", "sd", "var")
  stats = paste0("round(", funs, "(", sensor,", na.rm=TRUE), 2)")
  names(stats) = funs
  as.list(stats)

  probs = c("25", "50", "75")
  quants = paste0("round(quantile(", sensor, ", probs=.", probs, ", na.rm=TRUE), 2)")
  names(quants) = paste0("q", probs)
  as.list(quants)

  stats = do.call(c, list(stats, quants))

  summary = records %>% select_("sid", sensor) %>%
    group_by_("sid") %>%
    summarise_(.dots = stats)
  return(data.frame(summary))
}

am_makePlots = function(records, sensor, sids){

  # adding a month column for plots
  records = records %>%
    dplyr::mutate(month = as.factor(month(mtime)))


  density_plot = records %>%
    ggplot2::ggplot(data = .,
      x = records$mtime,
      y = records[sensor]) +
    aes_string(
      x = sensor,
      colour = colnames(records)[1]) +
    geom_density() # +
  # ggtitle(paste0(sensor.chr, "(", duration.chr, ") - ", "density"))

  timeSerie_plot = records %>%
    ggplot(data = .,
      x = records$mtime,
      y = records[sensor]) +
    aes_string(
      #x=colnames(records)[12],
      x = "mtime",
      y = sensor,
      colour = colnames(records)[1]
      # text = paste(
      #   'station :', records[3],
      #   '<br>Date: ', as.Date(records$mtime),
      #   '<br>Value: ', records[sensor_col.num]
      # )
    ) +
    geom_line() #+
  # ggtitle(paste0(sensor.chr, "(", duration.chr, ") - ", "time"))

  # mutate_at(records, "mtime", yday)
  # records[1] <- apply(records[1], 1, yday)

  recordsWide = records[c("mtime", "sid", "tsa")] %>%
    spread_("sid", "tsa")

  colnames(recordsWide)[-1] = paste0("station ", colnames(recordsWide)[-1])
  recordsWide = recordsWide %>%
    dplyr::mutate(month = as.factor(month(mtime)))

  scatter_plot = recordsWide %>%
    ggplot(
      data = .,
      aes_string(
        x = as.name(names(recordsWide)[2]),
        y = as.name(names(recordsWide)[3]),
        colour = as.name(names(recordsWide)[4])
    )) +
    geom_point() +
    geom_smooth(method = lm, color = "black", fill = "blue") +
    ggtitle("scatter plot : ", sensor)

  return(list(density_plot = density_plot,
    timeSerie_plot = timeSerie_plot,
    scatter_plot = scatter_plot
    ))

}

am_makeBlandAltman = function(records, sids){
  # transform to wideFormat for BlandAltman Analysis and remove times where at least one of the 2 stations as an NA value
  recordsWide = records[c("mtime", "sid", "tsa")] %>%
      spread_("sid", "tsa")


  # making ba analysis
  blandAlt = BlandAltmanLeh::bland.altman.stats(
    dplyr::select(records, sids[1])[[1]],
    dplyr::select(records, sids[2])[[1]])
  blandAltDf = data.frame(means = blandAlt$means, diffs = blandAlt$diffs)

  # group everythin to build the plots
  blandAltDf = bind_cols(blandAltDf, records["mtime"]) %>%
    dplyr::mutate(month = as.factor(month(mtime)))


  # create a BA plot
  ba_plot = ggplot(blandAltDf, aes(x = means, y = diffs, color = month)) +
    geom_point() +
    geom_smooth(method = glm, se = TRUE, color = "black", linetype = "dashed") +
    geom_hline(yintercept =  0, color = "black", size = 0.5) +
    geom_hline(yintercept = blandAlt$lines[2], color = "red", size = 0.5) +
    geom_hline(yintercept = blandAlt$lines[1], color = "blue", size = 0.5) +
    geom_hline(yintercept = blandAlt$lines[3], color = "blue", size = 0.5) +
    geom_hline(yintercept = blandAlt$CI.lines[1], linetype = "dashed", color = "blue", size = 0.5) +
    geom_hline(yintercept = blandAlt$CI.lines[2], linetype = "dashed", color = "blue", size = 0.5) +
    geom_hline(yintercept = blandAlt$CI.lines[5], linetype = "dashed", color = "blue", size = 0.5) +
    geom_hline(yintercept = blandAlt$CI.lines[6], linetype = "dashed", color = "blue", size = 0.5) +
    geom_hline(yintercept = blandAlt$CI.lines[3], linetype = "dashed", color = "red", size = 0.5) +
    geom_hline(yintercept = blandAlt$CI.lines[4], linetype = "dashed", color = "red", size = 0.5)

  # return a list
  return(list(ba_plot = ba_plot, blandAlt = blandAltDf))


}
