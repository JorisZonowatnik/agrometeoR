#' @export
#' @title make plots for a sensor of a station for a specific period of time
#' @author Thomas Goossens
#' @import mlr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @param records a list of hourly dataframes or a single dataframe ::TODO ::Must contain sunset and sunrise !
#' @param sensor a character vector specifying the name of the sensors you want to get the descriptive statistics
#' @param combs a character vector specifying the sid's combinations of the stations you want to put on the same plot
#' @param dfrom a character vector
#' @param dto a character vector
#' @return a list

makePlots = function(
  records,
  sensors,
  combs,
  dfrom = NULL,
  dto = NULL,
  interactive = TRUE){

  # from list of df's to big df
  records = dataframizeRecords(records)

  # extracting the unique sids
  sids = as.character(unique(records$sid))

  # check if sids available are compatible with the combs ::TODO::
  # stopifnot(combs %in% sids)

  # instancing the plot list
  plots = list(density = NULL,
    timeSerie = NULL,
    scatter = NULL,
    blandAltman = NULL
  )

  # check which sensors are available compared to those wanted
  sensors = checkSensors(records, sensors)

  # instancing the sensors list
  sensors = as.list(sensors)
  names(sensors) = sensors

  # settings the sids as factor for proper plotting
  records = records %>%
    dplyr::mutate(sid = as.factor(sid))

  # adding a color factor column for plots that depends of the time extent of the records.
  if (length(unique(records$month)) > 3) {
    scatterColors = "month"
  }
  else {
    if (length(unique(records$date)) > 14) {
      scatterColors = "week"
    }
    else {
      if (length(unique(records$date)) > 1) {
        scatterColors = "date"
      } else{
        scatterColors = "hour"
      }
    }
  }

  getSensorPlots = function(records, sensor, comb){

    # filter the records to only keep the sids that correspond to the comb
    records = records %>%
      dplyr::filter(sid %in% comb)

    # title for the plots
    title = paste0(sensor, " (", min(records$mtime), " ==> ", max(records$mtime), ")")

    # density
    plots$density = records %>%
      ggplot2::ggplot(data = .,
        x = records$mtime,
        y = records[sensor]) +
      aes_string(
        x = sensor,
        colour = "sid") +
      geom_density() +
      labs(caption = title) +
      theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))

    # timeSerie
    plots$timeSerie = records %>%
      ggplot(data = .,
        x = records$mtime,
        y = records[sensor]) +
      aes_string(
        x = "mtime",
        y = sensor,
        colour = "sid"
      ) +
      geom_line() +
      labs(caption = title) +
      theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))

    # # putting in wide format for scatter & blandAltman
    # recordsWide = records %>%
    #   dplyr::select(one_of(c("mtime", scatterColors, "sid", "tsa"))) %>%
    #   tidyr::spread_("sid", "tsa") %>%
    #   rename_at(vars(sids),function(x) paste0("station_", x))
    #
    # # scatter plot
    # plots$scatter = recordsWide %>%
    #   ggplot(
    #     data = .,
    #     aes_string(
    #       x = as.name(names(recordsWide)[3]),
    #       y = as.name(names(recordsWide)[4]),
    #       colour = as.name(names(recordsWide)[2])
    #     )) +
    #   geom_point() +
    #   geom_smooth(method = lm, color = "black", fill = "blue") +
    #   labs(caption = title) +
    #   theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))
    #
    # # bland-Altman plot
    # blandAlt = BlandAltmanLeh::bland.altman.stats(
    #   dplyr::select(recordsWide, paste0("station_", sids[1]))[[1]],
    #   dplyr::select(recordsWide, paste0("station_", sids[2]))[[1]])
    # blandAltDf = data.frame(means = blandAlt$means, diffs = blandAlt$diffs)
    # blandAltDf = data.frame(bind_cols(blandAltDf, recordsWide))
    #
    # plots$blandAltman = blandAltDf %>%
    #   ggplot(
    #     aes_string(
    #       x = as.name(names(blandAltDf)[1]),
    #       y = as.name(names(blandAltDf)[2]),
    #       colour = as.name(names(blandAltDf)[4]))
    #   ) +
    #   #colour = as.name(colnames(blandAltDf)[which(names(blandAltDf) %in% scatterColors)])) +
    #   geom_point() +
    #   geom_smooth(method = glm, se = TRUE, color = "black", linetype = "dashed") +
    #   geom_hline(yintercept =  0, color = "black", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$lines[2], color = "red", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$lines[1], color = "blue", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$lines[3], color = "blue", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$CI.lines[1], linetype = "dashed", color = "blue", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$CI.lines[2], linetype = "dashed", color = "blue", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$CI.lines[5], linetype = "dashed", color = "blue", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$CI.lines[6], linetype = "dashed", color = "blue", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$CI.lines[3], linetype = "dashed", color = "red", size = 0.5) +
    #   geom_hline(yintercept = blandAlt$CI.lines[4], linetype = "dashed", color = "red", size = 0.5) +
    #   labs(caption = title) +
    #   theme(plot.caption = element_text(hjust = 0.5, size = rel(1.2)))

    # interactivity ?
    if (isTRUE(interactive)) {
      plots = lapply(plots, plotly::ggplotly)
    }
    # return everything
    return(plots)
  }

  plotsBySensor =
    purrr::map2(
      .x = sensors,
      .y = combs,
      .f = getSensorPlots,
      records = records)
  browser()
  names(plotsBySensor) = sensors

  return(plotsBySensor)
}