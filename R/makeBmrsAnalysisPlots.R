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

    # making a single big df
    bmrsAnalysis = bmrsAnalysis %>%
      purrr::transpose()

    dataset_stations = bmrsAnalysis$summarized_by_sid %>%
      purrr::map_df(.,c, .id = "learner") %>%
      dplyr::mutate_at(vars("sid"), as.numeric)

    dataset_full = bmrsAnalysis$data_summary %>%
      purrr::map_df(.,c, .id = "learner") %>%
      dplyr::left_join(stations.df, by = "sid") %>%
      dplyr::left_join(dataset_stations, by = "sid") %>%
      dplyr::mutate_at(vars("sid", "poste", "datetime"), as.factor) %>%
      dplyr::rename("learner" = "learner.x") %>%
      dplyr::select(-c("learner.y"))


    # # making df for distribution vlines
    # Int_residuals_stations = dataset_full %>%
    #   group_by(sid) %>%
    #   summarize(Int_residuals_stations = mean(residuals))
    # Int_residuals_learners = dataset_full %>%
    #   group_by(learner) %>%
    #   summarize(Int_residuals_learners = mean(residuals))
    #
    # # joining to the original dataset_full
    # dataset_full = dataset_full %>%
    #   dplyr::left_join(Int_residuals_stations, by="sid") %>%
    #   dplyr::left_join(Int_residuals_learners, by="learner")

    # https://stackoverflow.com/questions/4946964/in-ggplot2-what-do-the-end-of-the-boxplot-lines-represent
    # https://www.r-bloggers.com/exploring-ggplot2-boxplots-defining-limits-and-adjusting-style/

    # function for boxplots
    doBoxPlot = function(param, group){
      bp = ggplot(dataset_full,
        aes_string(x=group, y= param, color = group)) +
        stat_boxplot(geom ='errorbar') +
        geom_boxplot(notch=FALSE) + stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
        theme(axis.text.x=element_text(angle = 45, hjust = 1), legend.position="none") +
        labs(title = paste0("Boxplot ", param, " by station"), x = "stations", y = param)# +
      if (param %in% c("rmse", "residuals")){
        if(group == "poste"){
          n_learners = seq(1,length(unique(dataset_full$learner)),1)
          names(n_learners) = sort(unique(dataset_full$learner))
          #  facet_wrap(learner ~ ., nrow = length(unique(dataset_full$learner)))
          bp = n_learners %>% purrr::map(., ~bp + ggforce::facet_grid_paginate(learner ~ ., ncol = 1, nrow=1, page = .))
        }
        if (group == "learner") {
          bp = bp +
            labs(title = paste0("boxplot global ", param, " by learner"), x = "learners", y = "rmse")
        }
      }
      return(bp)
    }

    # function for maps
    doMaps = function(param){
      m = makeLeafletMap(
        target = param,
        stations_data = dataset_full.summary,
        title = paste0("Map of ", param)
      )$outputvalue
      return(m)
    }

    # function for residuals scatters
    doScatterResiduals = function(group){
      sr = ggplot(dataset_full,
        aes_string("truth", "response")) +
        geom_point() +
        geom_smooth(se = FALSE) +
        geom_rug(color = "red") +
        ggtitle("True value vs. fitted value") +
        theme(axis.text.x=element_text(angle = 45, hjust = 1), legend.position="none")
      if (group == "learner") {
        sr = sr +
          facet_wrap(as.formula(paste(". ~", group)), ncol = 2)
      }
      if (group == "poste") {
        n_postes = seq(1,length(unique(dataset_full$poste)),1)
        names(n_postes) = sort(unique(dataset_full$poste))
        #  facet_wrap(learner ~ ., nrow = length(unique(dataset_full$learner)))
        sr = n_postes %>% purrr::map(., ~sr + ggforce::facet_grid_paginate(poste ~ ., ncol = 1, nrow=1, page = .))
        # sr = sr +
        #   facet_wrap(as.formula(paste(". ~", group)), ncol = 4)
      }
      return(sr)
    }

    # browser()
    groups = list(poste = "poste", learner = "learner")
    browser()
    boxPlots_rmse = purrr::map(groups, ~doBoxPlot("rmse", .))
    boxPlots_residuals = purrr::map(groups, ~doBoxPlot("residuals", .))
    boxPlots_observations = doBoxPlot("truth", "poste")
    boxPlots_predictions = purrr::map(groups, ~doBoxPlot("response", .))
    scatter_residuals = purrr::map(groups, ~doScatterResiduals(.))

    # summary for maps
    dataset_summ = dataset_full %>%

    # boxPlots = purrr::map(params, ~purrr::map2(., groups, doBoxPlot_perfs))
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




