#' @export
#' @title make plots for a sensor of a station for a specific period of time
#' @author Thomas Goossens
#' @import mlr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @param dataset a dataframe containing the records from the 2 stations you want to compare. ::TODO ::Must contain sunset and sunrise !
#' @param sensor a character specifying the name of the sensor you want to plot
#' @param sid a character specifying the sid of the station you want to plot
#' @return a list wihch elements are objects of class mlr::benchmark()

makePlots = function(records, sensor, sid, dfrom, dto, interactive = TRUE){

  # instancing the plot list
  plots = list(density = NULL,
    timeSerie = NULL,
    scatter = NULL,
    blandAltman = NULL
  )

  # settings the sids as factor for proper plotting
  records = records %>%
    dplyr::mutate(sid = as.factor(sid))

  # extracting the unique sids
  sids = as.character(unique(records$sid))

  # title for the plots
  title = paste0(sensor, " (", min(records$mtime), " ==> ", max(records$mtime), ")")

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

  # density
  plots$density = records %>%
    ggplot2::ggplot(data = .,
      x = records$mtime,
      y = records[sensor]) +
    aes_string(
      x = sensor,
      colour = "sid") +
    geom_density() +
    labs(caption=title) +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

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
    theme(plot.caption = element_text(hjust = 0.5, size=rel(1.2)))

  # putting in wide format for scatter & blandAltman
  recordsWide = records %>%
    dplyr::select(one_of(c("mtime", scatterColors, "sid", "tsa"))) %>%
    tidyr::spread_("sid", "tsa") %>%
    rename_at(vars(sids),function(x) paste0("station_", x))


  if (length(sids) > 2 ){

  }

  # scatter plot
  plots$scatter = recordsWide %>%
    ggplot(
      data = .,
      aes_string(
        x = as.name(names(recordsWide)[3]),
        y = as.name(names(recordsWide)[4]),
        colour = as.name(names(recordsWide)[2])
      )) +
    geom_point() +
    geom_smooth(method = lm, color = "black", fill = "blue") +
    labs(caption=title) +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

  # bland-Altman plot

  blandAlt = BlandAltmanLeh::bland.altman.stats(
    dplyr::select(recordsWide, paste0("station_", sids[1]))[[1]],
    dplyr::select(recordsWide, paste0("station_", sids[2]))[[1]])
  blandAltDf = data.frame(means = blandAlt$means, diffs = blandAlt$diffs)
  blandAltDf = data.frame(bind_cols(blandAltDf, recordsWide))

  plots$blandAltman = blandAltDf %>%
    ggplot(
      aes_string(
        x = as.name(names(blandAltDf)[1]),
        y = as.name(names(blandAltDf)[2]),
        colour = as.name(names(blandAltDf)[4]))
    ) +
    #colour = as.name(colnames(blandAltDf)[which(names(blandAltDf) %in% scatterColors)])) +
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
    geom_hline(yintercept = blandAlt$CI.lines[4], linetype = "dashed", color = "red", size = 0.5) +
    labs(caption = title) +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

  # interactivity ?
  if (isTRUE(interactive)) {
    plots = lapply(plots, plotly::ggplotly)
  }
  # return everything
  return(plots)
}