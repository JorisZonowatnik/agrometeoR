# sids = c("61", "1", "24")
# dfrom = "2017-11-01"
# dto = "2017-12-18"
# sensors = c("tsa", "ens", "vvt")
#
# # creating the dataset
# dataset = agrometeoR::makeDataset(
#   stations = sids,
#   dfrom = dfrom,
#   dto = dto,
#   sensor = c(sensors, "sunrise", "sunset"))
# datasetDf = do.call(rbind, dataset$output$value)
#
# makeComparison = function(dataset){
#
#   # removing potential NA values and raise warning
#   logNa = datasetDf[rowSums(is.na(datasetDf)) > 0,]
#   datasetDfNoNa = datasetDf[rowSums(is.na(datasetDf)) == 0,]
#   if (!is.null(nrow(logNa))) {
#     warning("Your dataset contains NA values that will be removed from the comparison.
#       These values were logged and are accessible")
#   }
#
#   # keeping only the commonly shared mtime by the 2 stations
#   datasetDfclean = subset(datasetDf, !as.character(mtime) %in% as.character(logNa$mtime))
#
#   # adding day or night
#   datasetDfDay = data.frame(
#     datasetDfclean %>%
#       rowwise() %>% dplyr::mutate(day = am_returnDayState(mtime, sunrise, sunset)))
#
#   # Add month/date/hour columns
#   datasetDfDayDmax = datasetDfDay %>%
#     dplyr::mutate(month = as.factor(lubridate::month(mtime))) %>%
#     dplyr::mutate(week = as.factor(lubridate::week(mtime))) %>%
#     dplyr::mutate(date = as.factor(lubridate::date(mtime))) %>%
#     dplyr::mutate(hour = as.factor(lubridate::hour(mtime)))
#
#   #datasetDfDayDmax = datasetDfDay
#
#   # add a dailyMax column
#   datasetDfDayDmax = data.frame(
#     datasetDfDayDmax %>%
#       dplyr::group_by(sid) %>%
#       dplyr::group_by(date) %>%
#       dplyr::mutate(daily_max = max(tsa)) %>%
#       dplyr::ungroup())
#
#   # Compute the summary stats for each sensor
#   output_stats = lapply(sensors, function(x){
#     am_makeStats(datasetDfDayDmax, x)
#   })
#   names(output_stats) = sensors
#
#   # # compute the blandAltman
#   # output_blandAltman = am_makeBlandAltman(
#   #   records = datasetDfDayDmax , sids = sids )
#
#   # compute the classic plots
#   output_plots = lapply(sensors, function(x){
#     am_makePlots(datasetDfDayDmax, x, TRUE)
#   })
#   names(output_plots) = sensors
#
#
#   }
#
#
# # declaration of the function to return a boolean for day state
# am_returnDayState = function(mtime, sunrise, sunset){
#   if (times(strftime(mtime,"%H:%M:%S")) >= sunrise && chron::times(strftime(mtime, format = "%H:%M:%S")) <= sunset){
#     day = TRUE
#   } else{
#     day = FALSE
#   }
#   return(day)
# }
#
# am_makeStats = function(records, sensor){
#
#   funs = c("sum", "mean", "min", "max", "sd", "var")
#   stats = paste0("round(", funs, "(", sensor,", na.rm=TRUE), 2)")
#   names(stats) = funs
#   as.list(stats)
#
#   probs = c("25", "50", "75")
#   quants = paste0("round(quantile(", sensor, ", probs=.", probs, ", na.rm=TRUE), 2)")
#   names(quants) = paste0("q", probs)
#   as.list(quants)
#
#   stats = do.call(c, list(stats, quants))
#
#   summary = records %>% select_("sid", sensor) %>%
#     group_by_("sid") %>%
#     summarise_(.dots = stats)
#   return(data.frame(summary))
# }
#
# am_makePlots = function(records, sensor, interactive = TRUE){
#
#   # instancing the plot list
#   plots = list(density = NULL,
#     timeSerie = NULL,
#     scatter = NULL,
#     blandAltman = NULL
#   )
#
#   # settings the sids as factor for proper plotting
#   records = records %>%
#     dplyr::mutate(sid = as.factor(sid))
#
#   # extracting the unique sids
#   sids = as.character(unique(records$sid))
#
#   # title for the plots
#   title = paste0(sensor, " (", min(records$mtime), " ==> ", max(records$mtime), ")")
#
#   # adding a color factor column for plots that depends of the time extent of the records.
#   if (length(unique(records$month)) > 3) {
#     scatterColors = "month"
#   }
#   else {
#     if (length(unique(records$date)) > 14) {
#       scatterColors = "week"
#     }
#     else {
#       if (length(unique(records$date)) > 1) {
#         scatterColors = "date"
#       } else{
#         scatterColors = "hour"
#       }
#     }
#   }
#
#   # density
#   plots$density = records %>%
#     ggplot2::ggplot(data = .,
#       x = records$mtime,
#       y = records[sensor]) +
#     aes_string(
#       x = sensor,
#       colour = "sid") +
#     geom_density() +
#     labs(caption=title) +
#     theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
#
#   # timeSerie
#   plots$timeSerie = records %>%
#     ggplot(data = .,
#       x = records$mtime,
#       y = records[sensor]) +
#     aes_string(
#       x = "mtime",
#       y = sensor,
#       colour = "sid"
#     ) +
#     geom_line() +
#     labs(caption = title) +
#     theme(plot.caption = element_text(hjust = 0.5, size=rel(1.2)))
#
#   # putting in wide format for scatter & blandAltman
#   recordsWide = records %>%
#     dplyr::select(one_of(c("mtime", scatterColors, "sid", "tsa"))) %>%
#     tidyr::spread_("sid", "tsa") %>%
#     rename_at(vars(sids),function(x) paste0("station_", x))
#
#
#   if (length(sids) > 2 ){
#
#   }
#
#   # scatter plot
#   plots$scatter = recordsWide %>%
#     ggplot(
#       data = .,
#       aes_string(
#         x = as.name(names(recordsWide)[3]),
#         y = as.name(names(recordsWide)[4]),
#         colour = as.name(names(recordsWide)[2])
#       )) +
#     geom_point() +
#     geom_smooth(method = lm, color = "black", fill = "blue") +
#     labs(caption=title) +
#     theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
#
#   # bland-Altman plot
#
#   blandAlt = BlandAltmanLeh::bland.altman.stats(
#     dplyr::select(recordsWide, paste0("station_", sids[1]))[[1]],
#     dplyr::select(recordsWide, paste0("station_", sids[2]))[[1]])
#   blandAltDf = data.frame(means = blandAlt$means, diffs = blandAlt$diffs)
#   blandAltDf = data.frame(bind_cols(blandAltDf, recordsWide))
#
#   plots$blandAltman = blandAltDf %>%
#     ggplot(
#       aes_string(
#         x = as.name(names(blandAltDf)[1]),
#         y = as.name(names(blandAltDf)[2]),
#         colour = as.name(names(blandAltDf)[4]))
#     ) +
#     #colour = as.name(colnames(blandAltDf)[which(names(blandAltDf) %in% scatterColors)])) +
#     geom_point() +
#     geom_smooth(method = glm, se = TRUE, color = "black", linetype = "dashed") +
#     geom_hline(yintercept =  0, color = "black", size = 0.5) +
#     geom_hline(yintercept = blandAlt$lines[2], color = "red", size = 0.5) +
#     geom_hline(yintercept = blandAlt$lines[1], color = "blue", size = 0.5) +
#     geom_hline(yintercept = blandAlt$lines[3], color = "blue", size = 0.5) +
#     geom_hline(yintercept = blandAlt$CI.lines[1], linetype = "dashed", color = "blue", size = 0.5) +
#     geom_hline(yintercept = blandAlt$CI.lines[2], linetype = "dashed", color = "blue", size = 0.5) +
#     geom_hline(yintercept = blandAlt$CI.lines[5], linetype = "dashed", color = "blue", size = 0.5) +
#     geom_hline(yintercept = blandAlt$CI.lines[6], linetype = "dashed", color = "blue", size = 0.5) +
#     geom_hline(yintercept = blandAlt$CI.lines[3], linetype = "dashed", color = "red", size = 0.5) +
#     geom_hline(yintercept = blandAlt$CI.lines[4], linetype = "dashed", color = "red", size = 0.5) +
#     labs(caption = title) +
#     theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
#
#   # interactivity ?
#   if (isTRUE(interactive)) {
#     plots = lapply(plots, plotly::ggplotly)
#   }
#   # return everything
#   return(plots)
# }
#
# # am_makeBlandAltman = function(records, sids){
# #   # transform to wideFormat for BlandAltman Analysis and remove times where at least one of the 2 stations as an NA value
# #   recordsWide = records[c("mtime", "sid", "tsa")] %>%
# #       tidyr::spread_("sid", "tsa")
# #
# #   # making ba analysis
# #   blandAlt = BlandAltmanLeh::bland.altman.stats(
# #     dplyr::select(recordsWide, sids[1])[[1]],
# #     dplyr::select(recordsWide, sids[2])[[1]])
# #   blandAltDf = data.frame(means = blandAlt$means, diffs = blandAlt$diffs)
# #
# #   # group everythin to build the plots
# #   blandAltDf = bind_cols(blandAltDf, recordsWide["mtime"])
# #   # %>%
# #   #   dplyr::mutate(month = as.factor(lubridate::month(mtime)))
# #
# #
# #   # create a BA plot
# #   ba_plot = ggplot(blandAltDf, aes(x = means, y = diffs, color = month)) +
# #     geom_point() +
# #     geom_smooth(method = glm, se = TRUE, color = "black", linetype = "dashed") +
# #     geom_hline(yintercept =  0, color = "black", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$lines[2], color = "red", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$lines[1], color = "blue", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$lines[3], color = "blue", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$CI.lines[1], linetype = "dashed", color = "blue", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$CI.lines[2], linetype = "dashed", color = "blue", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$CI.lines[5], linetype = "dashed", color = "blue", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$CI.lines[6], linetype = "dashed", color = "blue", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$CI.lines[3], linetype = "dashed", color = "red", size = 0.5) +
# #     geom_hline(yintercept = blandAlt$CI.lines[4], linetype = "dashed", color = "red", size = 0.5)
# #
# #   # return a list
# #   return(list(ba_plot = ba_plot, blandAlt = blandAltDf))
# # }
