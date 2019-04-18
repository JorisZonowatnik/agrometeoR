#' @export
#' @title Plot analyse batch of benchmark experiments output
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param bmrsExtraction a dataframe
#' @return a list of plots

makeBmrsPLots <- function(
  bmrsExtraction){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doPlots = function(){

    dataset = bmrsExtraction %>%
      dplyr::left_join(stations.df, by = "sid") %>%
      dplyr::mutate_at(vars("sid", "poste"), as.factor)

    if (length(dataset$datetime[1]) > 8){
      dataset = dataset %>%
        dplyr::mutate_at(vars("datetime"), lubridate::ymd_hms)
    }else{
      dataset = dataset %>%
        dplyr::mutate_at(vars("datetime"), lubridate::ymd)
    }


    # # making df for distribution vlines
    # Int_residuals_stations = dataset %>%
    #   group_by(sid) %>%
    #   summarize(Int_residuals_stations = mean(residuals))
    # Int_residuals_learners = dataset %>%
    #   group_by(learner) %>%
    #   summarize(Int_residuals_learners = mean(residuals))
    #
    # # joining to the original dataset
    # dataset = dataset %>%
    #   dplyr::left_join(Int_residuals_stations, by="sid") %>%
    #   dplyr::left_join(Int_residuals_learners, by="learner")

    # https://stackoverflow.com/questions/4946964/in-ggplot2-what-do-the-end-of-the-boxplot-lines-represent
    # https://www.r-bloggers.com/exploring-ggplot2-boxplots-defining-limits-and-adjusting-style/

    # function for boxplots
    # https://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot
    doBoxPlot = function(param, group){

      # filtering the NA values
      bp = ggplot(dataset,
        aes_string(x=group, y= param, color = group)) +
        stat_boxplot(geom ='errorbar') +
        geom_boxplot(notch=FALSE, outlier.shape = NA) + stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) +
        scale_y_continuous(limits = quantile(dataset[[param]], c(0.1, 0.9), na.rm = TRUE)) +
       # theme(axis.text.x=element_text(angle = 45, hjust = 1), legend.position = "none") +
        labs(title = paste0("Boxplot ", param, " by station"), x = "stations", y = param)# +
      if (param %in% c("rmse", "residuals")){
        if(group == "poste"){
          n_learners = seq(1,length(unique(dataset$learner)),1)
          names(n_learners) = sort(unique(dataset$learner))
          facet_wrap(learner ~ ., nrow = length(unique(dataset$learner)))
          #bp = n_learners %>% purrr::map(., ~bp + ggforce::facet_grid_paginate(learner ~ ., ncol = 1, nrow=1, page = .))
        }
        if (group == "learner") {
          bp = bp +
            labs(title = paste0("boxplot global ", param, " by learner"), x = "learners", y = param)
        }
      }
      return(bp)
    }

    doBoxPlotStats = function(param, group){
      doStatsBylearner = function(l){
        dataset = dataset %>%
          dplyr::filter(learner == l )
        stats = as.data.frame(as.list(boxplot.stats(dataset[[param]])$stats))
        colnames(stats) = c("min", "q25", "median", "q75", "max")
        return(stats)
      }
      learners = unique(dataset$learner)
      statsByLearner = learners %>% purrr::map(., doStatsBylearner)
      names(statsByLearner) = learners

      return(statsByLearner)
    }

    # function for timeseries
    n_learners = seq(1,length(unique(dataset$learner)),1)
    names(n_learners) = sort(unique(dataset$learner))

    doTimeSerie = function(param, group, global = TRUE){
      # doTimeSerieByStation = function(p){
      #   df = dataset %>% dplyr::filter(poste %in% p)
      #   ggplot2::ggplot(data = df) +
      #     geom_line(aes_string(x = "datetime", y = param, color = group),
      #       alpha = 0.6,
      #       size = 0.6) +
      #     labs(x = "Datetime",
      #       y = param,
      #       title = paste0("Timeserie of ", param, " for station ",  p, " by ", group)) +
      #     theme_minimal()
      # }

      if (!isTRUE(global)) {
        postes = unique(dataset$poste)
        timeseries_by_sid = postes %>% purrr::map(doTimeSerieByStation)
        names(timeseries_by_sid) = postes
      }
      else{
        summary_by_learner = dataset %>%
          dplyr::group_by_("datetime", group) %>%
          dplyr::summarise_at(param, list(min = min, max = max, mean = mean, median = median, var = var))

        ts = ggplot2::ggplot(data = summary_by_learner) +
          geom_line(aes_string(x = "datetime", y = "mean", color = "learner"),
            alpha = 0.6,
            size = 0.6) +
          labs(x = "Datetime",
            y = paste0("mean(",param,")"),
            title = paste0("Timeserie of mean ", param, " by learner")) +
          theme_minimal()

      }
      return(ts)
    }

    # # function for maps
    doLeafletMap = function(dataset){
      params = colnames(dataset)
      dataset = dataset %>% mutate_at("sid", as.numeric)
      maps = params %>% purrr::map(
      ~makeLeafletMap(
        target = .,
        spatialized = NULL,
        polygon_grid = grid.squares.sf,
        stations_data = dataset,
        stations_meta = stations.df,
        key_grid = "px",
        key_stations = "sid",
        stations_coords = c("x", "y"),
        crs = 3812,
        title))
    }

    # function for residuals scatters
    doScatterResiduals = function(group){
      sr = ggplot(dataset,
        aes_string("truth", "response")) +
        geom_point() +
        geom_smooth(se = FALSE) +
        geom_rug(color = "red") +
        ggtitle("True value vs. fitted value") +
        theme(legend.position="none")
      if (group == "learner") {
        sr = sr +
          facet_wrap(as.formula(paste(". ~", group)), ncol = 2)
      }
      if (group == "poste") {
        n_postes = seq(1,length(unique(dataset$poste)),1)
        names(n_postes) = sort(unique(dataset$poste))
        #  facet_wrap(learner ~ ., nrow = length(unique(dataset$learner)))
        sr = n_postes %>% purrr::map(., ~sr + ggforce::facet_grid_paginate(poste ~ ., ncol = 1, nrow=1, page = .))
        sr = sr +
          facet_wrap(as.formula(paste(". ~", group)), ncol = 4)
      }
      return(sr)
    }


    #groups = list(poste = "poste", learner = "learner")
    groups = list(learner = "learner")
    boxPlots_rmse = purrr::map(groups, ~doBoxPlot("rmse", .))[[1]]
    boxPlots_residuals = purrr::map(groups, ~doBoxPlot("residuals", .))[[1]]

    scatter_residuals = purrr::map(groups, ~doScatterResiduals(.))
    stats_residuals = purrr::map(groups, ~doBoxPlotStats("residuals", .))
    stats_rmse = purrr::map(groups, ~doBoxPlotStats("rmse", .))
    # boxPlots_observations = doBoxPlot("truth", "poste")
    # boxPlots_predictions = purrr::map(groups, ~doBoxPlot("response", .))
    timeSeries_residuals = doTimeSerie("residuals", "learner")
    timeSeries_rmse = doTimeSerie("rmse", "learner")
    #timeSeries_rmse = doTimeSerie("rmse", "learner")
    # summaries for maps
    #dataTomap = bmrsExtraction$summarized_by_sid
    # maps = dataTomap %>% purrr::map(doLeafletMap)


    # boxPlots = purrr::map(params, ~purrr::map2(., groups, doBoxPlot_perfs))
    # globalPlots = bmrsResult %>% mlr::plotBMRBoxplots(pretty.names = FALSE)
    # learnersPlots = bmrsExtraction %>% purrr::map(~makeLearnerPlots(.))

    # https://stackoverflow.com/questions/44196384/how-to-produce-different-geom-vline-in-different-facets-in-r
    # https://stackoverflow.com/questions/39736655/ggplot2-plots-over-multiple-pageshttps://stackoverflow.com/questions/39736655/ggplot2-plots-over-multiple-pages

    # Throw a success message
    message("Success, plots created")

    # return all the plots
    return(list(
      stats_residuals = stats_residuals,
      stats_rmse = stats_rmse,
      boxPlots_rmse = boxPlots_rmse,
      boxPlots_residuals = boxPlots_residuals,
      timeSeries_residuals = timeSeries_residuals,
      timeSeries_rmse = timeSeries_rmse
      # scatter_residuals = scatter_residuals
      #timeSeries_residuals = timeSeries_residuals
    ))

  }

  tryCatch(
    expr = {


      # in case everything went fine, do makebmrsExtractionPLots
      output$value = doPlots()
      output$condition$type = "success"
      output$condition$message = "Plots created"
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeBmrsPLots raised a warning -> ",
        w)
      snitch <<- TRUE
      output$value <<- doPlots()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::makeBmrsPLots raised an error -> ",
        e)
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "makebmrsExtractionPLots has encountered : ",
        output$condition$type,
        ". \n",
        "All done with makeBmrsPLots "
      )
      message(finalMessage)
      return(list(snitch = snitch, output = output))
    }
  )
}




