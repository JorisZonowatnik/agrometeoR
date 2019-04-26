#' @export
#' @title Compute and extract relevant information from a batch of benchmark experiments. This extraction can then be used to produce various plots.
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param benchmarkResult an object of class \code{mlr::benchmark()} produced by \code{agrometeoR::makeBrmsBatch}
#' @param tasks a list which elements are of class \code{mlr::makeRegrTask()}. These tasks must be the same that were passed to makeBmrsBatch
#' @param tasks_sids a list which elements are integer vectors containing the sids of the used stations of each task. This list is provided as an output element of the \code{agrometeoR::makeTaks()} function.
#' @param as.df a boolean specifying if the result must be returned as a single dataframe rather than list of lists. Default is \code{TRUE}
#' @return A 2 elements named list
#' \itemize{
#'   \item \code{snitch} : a boolean. Is \code{TRUE} if function has provided the expected result. Is \code{FALSE} is function throws an error
#'   \item \code{output} : a named list which elements are : \itemize{
#'     \item \code{value} : an element of class \code{data.frame} if parameter \code{as.df} is set to \code{TRUE}. If set to \code{FALSE}, the function returns a list which elements are dataframes
#'     \item \code{condition} : a character specifying the condition encountered by the function : success, warning, or error.
#'     \item \code{message} : a character specifying the message relative to the condition.
#'     }
#'  }
#' @examples
#'\dontrun{
#' # load magrittr for pipe use : %>%
#' library(magrittr)
#'
#' # create the dataset
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T18:00:00Z",
#'   sensor = "tsa")
#'
#' # extract the list of hourly sets of records
#' myDataset = myDataset$output$value
#'
#' # create the tasks
#' myTasks = purrr::map(myDataset, makeTask, target = "tsa")
#'
#'#' # extract the used sids of each task from the outputs
#' myUsedSids = myTasks %>% purrr::modify_depth(1, ~.$output$stations$used)
#'
#' # extract the tasks from the outputs
#' myTasks = myTasks %>% purrr::modify_depth(1, ~.$output$value$task)
#'
#' # Conduct a batch of benchmarks experiments without saving temp files
#'myBmrsBatch = makeBmrsBatch(
#' tasks = myTasks,
#' learners = agrometeorLearners,
#' measures = list(mlr::rmse),
#' keep.pred = TRUE,
#' models = FALSE,
#' groupSize  = NULL,
#' level = "mlr.benchmark",
#' resamplings = "LOO",
#' cpus = 1,
#' prefix = NULL,
#' temp_dir = NULL,
#' removeTemp = FALSE)
#'
#' # Keep the relevant information
#' myBmrsBatch = myBmrsBatch$output$value
#'
#' # Get the extraction from myBmrsBatch
#' myBrmsExtraction = makeBmrsExtraction(myBmrsBatch, myTasks, myUsedSids, as.df = TRUE)
#'
#' # Get an excerpt of the output
#' head(myBmrsExtraction$output$value)
#' }


makeBmrsExtraction <- function(
  benchmarkResult,
  tasks,
  tasks_sids,
  as.df = TRUE){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doExtraction = function(){
    #purrr solutions inspired from slack https://r-grrr.slack.com by https://www.benjaminlouis-stat.fr/

    ## Get preds and perfs and transposing to sort by learner
    benchmark_preds = benchmark_preds %>% purrr::transpose() #transposing to sort by learners rather than by date
    benchmark_perfs =  benchmark_perfs %>% purrr::transpose()

    # computing the residuals for each set of records preds
    get_res <- function(l) {

      cv_data = l$data
      cv_data = cv_data %>%
        dplyr::mutate(residuals = response - truth) # %>%
      # dplyr::mutate(res_sqrtsq = sqrt(residuals*residuals))
      return(cv_data)

    }
    residuals = benchmark_preds %>%
      purrr::modify_depth(2, get_res)

    # getting the sids of the stations used for each set of records
    get_used_sids = function(l){
      used = data.frame(tasks_sids)
      # used = data.frame(l$output$stations$used)
      colnames(used) = "sid"
      return(used)
    }

    used_sids = tasks %>% purrr::modify_depth(1, get_used_sids)

    # colbinding the sids to the residuals
    bind_sids = function(l1, l2){
      l = l1 %>%
        purrr::map2(l2, dplyr::bind_cols)
    }
    residuals = residuals %>%
      purrr::modify_depth(1, bind_sids, used_sids)

    # colbinding the perfs to the residuals
    bind_perfs = function(l1, l2) {
      test = purrr::map2(l1, l2, dplyr::bind_cols)
      test = purrr::map(test, ~ dplyr::select(., -c("iter1", "set")))
      return(test)
    }
    dataset = purrr::map2(benchmark_perfs, residuals, bind_perfs)

    #####
    ## making a single big dataframe for plots
    if (isTRUE(as.df)){
      group_all_datetimes = function(l){
        df = l %>%
          dplyr::bind_rows(.id = "datetime")
      }
      dataset = dataset %>%
        purrr::modify_depth(1, group_all_datetimes) %>%
        purrr::map_df(.,c, .id = "learner")
    }

    # Throw a success message
    message("Success, Analysis of batch of benchmark experiments performed")

    # return the result
    return(dataset)

  }

  tryCatch(
    expr = {

      benchmark_preds = mlr::getBMRPredictions(benchmarkResult)
      benchmark_perfs =  mlr::getBMRPerformances(benchmarkResult)

      if (!identical(names(tasks), getBMRTaskIds(benchmarkResult))){
        tasks = tasks %>% purrr::keep(., names(.) %in% names(getBMRTaskIds(benchmarkResult)))

        # benchmark_perfs = benchmark_perfs %>% purrr::keep(., names(.) %in% names(tasks))
        # benchmark_preds = benchmark_preds %>% purrr::keep(., names(.) %in% names(tasks))
        warning(paste0(
          "Some of your tasks do not have a corresponding benchmark result. Only the matching ones have been kept"
        ))
      }

      # in case everything went fine, doExtraction
      output$value = doExtraction()
      output$condition$type = "success"
      output$condition$message = "Analysis performed"
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::analyzeBatchOfBenchExp raised a warning -> ",
        w)
      snitch <<- TRUE
      output$value <<- doExtraction()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::analyzeBatchOfBenchExp raised an error -> ",
        e)
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "analyzeBatchOfBenchExp has encountered : ",
        output$condition$type,
        ". \n",
        "All done with analyzeBatchOfBenchExp "
      )
      message(finalMessage)
      return(list(snitch = snitch, output = output))
    }
  )
}




