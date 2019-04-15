#' @export
#' @title analyse batch of benchmark experiments output
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param benchmarkResult a list of class BenchmarkResult returned in the outputs of analyzeBatchOfBenchExp
#' @param tasks a list of the mlr taks that were passed to analyzeBatchOfBenchExp
#' @param as.df a boolean specifying if the result must be returned as a single df rather than list of lists
#' @return a list containing relevant performance indications by learners

makeBmrsExtraction <- function(
  benchmarkResult,
  tasks,
  as.df = TRUE){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doAnalysis = function(){
    #purrr solutions inspired from slack https://r-grrr.slack.com by https://www.benjaminlouis-stat.fr/


    #####
    ## computing the residuals for each iteration of CV of each datetime of each learner
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
        test = purrr::map2(l1, l2, dplyr::bind_cols)
        test = purrr::map(test, ~ dplyr::select(., -c("iter1", "set")))
        return(test)
      }
      dataset = purrr::map2(benchmark_perfs, residuals, bind_perfs)

    #####
    ## globally summarizing each learner by datetime on all the stations
    summarize_by_datetime = function(l){
      summarize = function(df){
        summary = df %>%
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
            stdev_pred = sd(response),
            min_rmse = min(rmse),
            max_rmse = max(rmse),
            mean_rmse = mean(rmse),
            min_abs_res = mean(abs(residuals), na.rm = TRUE),
            max_abs_res = max(abs(residuals), na.rm = TRUE),
            mean_abs_res = min(abs(residuals), na.rm = TRUE),
            mean_se = mean(se, na.rm = TRUE))
      }
      summarizes = l %>%
        purrr::map(summarize)
      summarizes.df = map2_df(summarizes, names(summarizes), ~purrr::update_list(.x, datetime = .y))
      return(summarizes.df)
    }
    summarized_by_datetime = dataset %>%
      purrr::modify_depth(1, summarize_by_datetime)


    #####
    ## globally summarizing each learner by stations on all the datetimes
    transpose_sid_date = function(l){
      l =  purrr::map2(l, names(l), ~purrr::update_list(.x, datetime = .y))
      l = l %>%
        dplyr::bind_rows() %>%
        dplyr::group_split(sid, keep = TRUE)
      names(l) = purrr::map(l, ~unique(.$sid))
      return(l)
    }

    dataset_by_sid = dataset %>%
      purrr::modify_depth(1, transpose_sid_date)
    summarize_by_sid = function(l){
      summarize = function(df){
        summary = df %>%
          dplyr::group_by(sid) %>%
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
            stdev_pred = sd(response),
            min_rmse = min(rmse),
            max_rmse = max(rmse),
            mean_rmse = mean(rmse),
            min_abs_res = mean(abs(residuals), na.rm = TRUE),
            max_abs_res = max(abs(residuals), na.rm = TRUE),
            mean_abs_res = min(abs(residuals), na.rm = TRUE),
            mean_se = mean(se, na.rm = TRUE))
      }
      summarizes = l %>%
        purrr::map(summarize)
      summarizes.df = purrr::map2_df(summarizes, names(summarizes), ~purrr::update_list(.x, sid = .y))
      return(summarizes.df)
    }
    summarized_by_sid = dataset_by_sid %>%
      purrr::modify_depth(1, summarize_by_sid)

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

    #####
    ##return all the analysis results in a list
    analysis = list(
      dataset = dataset,
      summarized_by_sid = summarized_by_sid,
      summarized_by_datetime = summarized_by_datetime
    )

  }

  tryCatch(
    expr = {

      # check if passed tasks and bmr outputs are corresponding
      stopifnot(identical(getBMRTaskIds(benchmarkResult), names(tasks)))

      # in case everything went fine, do analyzeBatchOfBenchExp
      output$value = doAnalysis()
      output$condition$type = "success"
      output$condition$message = "Analysis performed"
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::analyzeBatchOfBenchExp raised a warning -> ",
        w)
      snitch <<- TRUE
      output$value <<- doAnalysis()
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




