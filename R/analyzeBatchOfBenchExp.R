#' @export
#' @title analyse batch of benchmark experiments output
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param benchmarkResult a list of class BenchmarkResult returned in the outputs of makeBatchOfBenchExp
#' @param tasks a list of the mlr taks that were passed to makeBatchOfBenchExp
#' @return a list containing relevant performance indications by learners

analyzeBatchOfBenchExp <- function(
  benchmarkResult,
  tasks){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doAnalysis = function(){

    # purrr solutions inspired from slack https://r-grrr.slack.com by https://www.benjaminlouis-stat.fr/

    #####
    ## working on the hourly/daily sets
    benchmark_preds = getBMRPredictions(benchmarkResult) %>% purrr::transpose() #transposing to sort by learners rather than by date
    benchmark_perfs = getBMRPerformances(benchmarkResult) %>% purrr::transpose()

    # benchmark_aggPerfs = getBMRAggrPerformances(benchmarkResult, as.df = FALSE) %>% purrr::transpose()

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
      used = data.frame(l$output$stations$used)
      colnames(used) = "sid"
      return(used)
    }
    used_sids = tasks %>% purrr::modify_depth(1, get_used_sids)

    # colbinding the sids to the residuals for later global summary
    bind_sids = function(l1, l2){
      l = l1 %>%
        purrr::map2(l2, bind_cols)
    }
    residuals = residuals %>%
      purrr::modify_depth(1, bind_sids, used_sids)

    # colbinding the perfs to the residuals for later global summary
    bind_perfs = function(l1, l2) {
      test = map2(l1, l2, dplyr::bind_cols)

      return(test)
    }
    test = map2(benchmark_perfs, residuals, bind_perfs)
    browser()



    # summarizing each set of records regarding min max, residuals, etc
    summarize_by_set = function(l){
      summarize = function(set){
        summary = set %>%
          dplyr::summarise(
            min_obs = min(truth),
            min_pred = min(response),
            min_diff_op = min(truth) - min(response),
            min_ratio_op = min(truth)/min(response),
            max_obs = max(truth),
            max_pred = max(response),
            max_diff_op = max(truth) - max(response),
            max_ratio_op = max(truth)/max(response),
            mean_obs = mean(truth),
            mean_pred = mean(response),
            var_obs = var(truth),
            var_pred = var(response),
            stdev_obs = sd(truth),
            stdev_pred = sd(response)
        )
      }
      summarizes = l %>%
        purrr::map(summarize)
      return(summarizes)
    }
    summarizes = residuals %>%
      purrr::modify_depth(1, summarize_by_set)


    # globally summarizing residuals by stations on all the sets of records
    summarize_by_sid = function(df){

      df = df %>%
        dplyr::group_by(sid) %>%
        dplyr::summarise(
          mean_obs = mean(response, na.rm = TRUE),
          mean_pred = mean(response, na.rm = TRUE),
          mean_abs_res = mean(abs(residuals), na.rm = TRUE),
          mean_se = mean(se, na.rm = TRUE)) %>%
        dplyr::left_join(stations.df, by = "sid")
    }
    residuals_summary = residuals %>%
      purrr::modify_depth(1, dplyr::bind_rows) %>%
      purrr::modify_depth(1, summarize_by_sid)


    #####
    ## working on performance indicators - rmse
    benchmark_perfs = getBMRPerformances(benchmarkResult) %>% purrr::transpose()
    aggPerfs = getBMRAggrPerformances(benchmarkResult, as.df = TRUE)

     rmse_summary = aggPerfs %>%
      dplyr::group_by(learner.id) %>%
      dplyr::select(rmse.test.rmse) %>%
      dplyr::summarise_all(
        funs(count = sum(!is.na(.)),
          min = min(.,na.rm = TRUE),
          max = max(.,na.rm = TRUE),
          mean = mean(.,na.rm = TRUE),
          median = median(.,na.rm = TRUE),
          sd = sd(.,na.rm = TRUE)))

     # Throw a success message
    message("Success, Analysis of batch of benchmark experiments performed")

    # return all the bmr results in a list
    return(analysis = list(
      rmse_summary = rmse_summary,
      residuals_summary = residuals_summary
    ))

  }

  tryCatch(
    expr = {
      # add checks
      # ::TODO:

      # in case everything went fine, do makeBatchOfBenchExp
      output$value = doAnalysis()
      output$condition$type = "success"
      output$condition$message = "Analysis performed"
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeBatchOfBenchExp raised a warning -> ",
        w)
      snitch <<- TRUE
      output$value <<- doAnalysis()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::makeBatchOfBenchExp raised an error -> ",
        e)
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "makeBatchOfBenchExp has encountered : ",
        output$condition$type,
        ". \n",
        "All done with makeBatchOfBenchExp "
      )
      message(finalMessage)
      return(list(snitch = snitch, output = output))
    }
  )
  }




