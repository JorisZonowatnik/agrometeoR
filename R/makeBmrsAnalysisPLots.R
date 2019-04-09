#' @export
#' @title Plot analyse batch of benchmark experiments output
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param bmrsAnalysis a list of class BenchmarkResult returned in the outputs of makeBmrsAnalysisPLots
#' @return a list of plots

makeBmrsAnalysisPLots <- function(
  bmrsAnalysis){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doPlots = function(){

    print("ahah")


#
#     bmrsAnalysis = bmrsAnalysis %>%
#       dplyr::left_join(stations.df, by = "sid") %>%
#       dplyr::mutate_at(vars("sid", "poste", "datetime"), as.factor)


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




