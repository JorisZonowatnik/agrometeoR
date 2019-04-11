#' @export
#' @title Plot analyse batch of benchmark experiments output
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param bmrsAnalysis a list of class BenchmarkResult returned in the outputs of makeBmrsAnalysisPLots
#' @return a list of plots

makeBmrsAnalysisPlots <- function(
  bmrsAnalysis){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doPlots = function(){

    makeBoxPlots = function(l, learner){
      browser()
      df = l$data_summary
      df = df %>%
        dplyr::left_join(stations.df, by = "sid") %>%
        dplyr::mutate_at(vars("sid", "poste", "datetime"), as.factor)

      indicators = list("rmse", "truth", "response", "se", "residuals", "abs_red")

      doBoxPlot = function(df, x, y, name){
        print(name)
        ggplot2::ggplot(df, aes(x = x, y = y)) +
          geom_boxplot(notch = FALSE) + stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) +
          labs(title = "boxplot",x = x , y = y) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        return(boxplot)
      }
      all_plots = purrr::pmap(.x = indicators, .y = df, doBoxPlot, name = learner)

    }

    plots = bmrsAnalysis %>% purrr::imap(makeBoxPlots)



    # Throw a success message
    message("Success, plots created")

    # return all the analysis results in a list
    plots = list(
      data_summary = data_summary,
      summarized_by_sid = summarized_by_sid,
      summarized_by_datetime = summarized_by_datetime
    ) %>% purrr::transpose()

    return(plots)

  }

  tryCatch(
    expr = {

      # in case everything went fine, do makeBmrsAnalysisPLots
      output$value = doPlots()
      output$condition$type = "success"
      output$condition$message = "Analysis performed"
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




