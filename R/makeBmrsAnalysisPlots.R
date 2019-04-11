#' @export
#' @title Plot analyse batch of benchmark experiments output
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param bmrsResult an object of class mlr::benchmarkResult
#' @param bmrsAnalysis a list of class BenchmarkResult returned in the outputs of makeBmrsAnalysisPLots
#' @return a list of plots

makeBmrsAnalysisPlots <- function(
  bmrsResult,
  bmrsAnalysis){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doPlots = function(){

    browser()

    # making a single big df
    dataset = bmrsAnalysis %>%
      purrr::transpose()
    dataset = dataset$data_summary %>%
      purrr::map_df(.,c, .id = "learner") %>%
      dplyr::left_join(stations.df, by = "sid") %>%
      dplyr::mutate_at(vars("sid", "poste", "datetime"), as.factor)

    # making df for distribution vlines
    dataInt_residuals_stations = dataset %>%
      group_by(sid) %>%
      summarize(Int_residuals_stations = mean(residuals))

    dataset = dataset %>%
      dplyr::left_join(dataInt_residuals_stations, by="sid")

    boxplot_rmse_stations = ggplot(dataset,
      aes_string(x="poste", y="rmse")) +
      geom_boxplot(notch=FALSE) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
      labs(title = "rmse", x = "stations", y = "rmse") +
      theme(axis.text.x=element_text(angle=45, hjust=1))

    boxplot_rmse_learners = ggplot(dataset,
      aes_string(x="learner", y="rmse")) +
      geom_boxplot(notch=FALSE) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
      labs(title = "rmse", x = "learners", y = "rmse") +
      theme(axis.text.x=element_text(angle=45, hjust=1))

    boxplot_residuals_stations = ggplot(dataset,
      aes_string(x="poste", y="residuals")) +
      geom_boxplot(notch=FALSE) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
      labs(title = "residuals", x = "stations", y = "residuals") +
      theme(axis.text.x=element_text(angle=45, hjust=1))

    boxplot_residuals_learners = ggplot(dataset,
      aes_string(x="learner", y="residuals")) +
      geom_boxplot(notch=FALSE) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
      labs(title = "residuals", x = "learners", y = "residuals") +
      theme(axis.text.x=element_text(angle=45, hjust=1))

    hist_residuals_learners = ggplot(dataset,
      aes_string(x = "residuals")) +
      geom_histogram(fill="white", color = "black") +
      geom_vline(aes(xintercept=mean(residuals)), color="blue",
        linetype="dashed") +
      geom_density() +
      ggtitle("Histogram of residuals by learner") +
      facet_wrap(learner ~ ., ncol = 2)

    hist_residuals_stations = ggplot(dataset,
      aes_string(x = "residuals", color = "learner")) +
      geom_histogram(fill="white", position="dodge") +
      geom_density() +
      geom_vline(aes(xintercept=Int_residuals_stations), color="blue",
        linetype="dashed") +
      ggtitle("Histogram of residuals by station") +
      ggforce::facet_grid_paginate(poste ~ ., ncol = 3, nrow=3, page = 1)

    hist_residuals_stations = ggplot(dataset,
      aes_string(x = "residuals", color = "learner")) +
      geom_histogram(fill="white", position="dodge") +
      geom_density() +
      geom_vline(aes(xintercept=Int_residuals_stations), color="blue",
        linetype="dashed") +
      ggtitle("Histogram of residuals by station") +
      ggforce::facet_grid_paginate(poste ~ learner, ncol = 5, nrow=3, page = 1)


    scatter_residuals_learners = ggplot(dataset,
      aes_string("truth", "response")) +
      geom_point() +
      geom_smooth(se = FALSE) +
      geom_rug(color = "red") +
      ggtitle("True value vs. fitted value") +
      facet_wrap(. ~ learner, ncol=2)

    scatter_residuals_stations = ggplot(dataset,
      aes_string("truth", "response")) +
      geom_point() +
      geom_smooth(se = FALSE) +
      geom_rug(color = "red") +
      ggtitle("True value vs. fitted value") +
      facet_wrap(. ~ poste, scales = "free", ncol=4)


    # globalPlots = bmrsResult %>% mlr::plotBMRBoxplots(pretty.names = FALSE)
    # learnersPlots = bmrsAnalysis %>% purrr::map(~makeLearnerPlots(.))

    # https://stackoverflow.com/questions/44196384/how-to-produce-different-geom-vline-in-different-facets-in-r
    # https://stackoverflow.com/questions/39736655/ggplot2-plots-over-multiple-pageshttps://stackoverflow.com/questions/39736655/ggplot2-plots-over-multiple-pages

    # Throw a success message
    message("Success, plots created")

    # return all the plots
    return(plots)

  }

  tryCatch(
    expr = {


      # in case everything went fine, do makeBmrsAnalysisPLots
      output$value = doPlots()
      output$condition$type = "success"
      output$condition$message = "Plots created"
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeBmrsAnalysisPLots raised a warning -> ",
        w)
      snitch <<- TRUE
      output$value <<- doPlots()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::makeBmrsAnalysisPLots raised an error -> ",
        e)
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "makeBmrsAnalysisPLots has encountered : ",
        output$condition$type,
        ". \n",
        "All done with makeBmrsAnalysisPLots "
      )
      message(finalMessage)
      return(list(snitch = snitch, output = output))
    }
  )
}




