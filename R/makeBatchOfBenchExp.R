#' @export
#' @title make a multicore parallelized mlr benchmark experiment for various learners on multiple tasks
#' @author Thomas Goossens
#' @import mlr
#' @import parallelMap
#' @importFrom magrittr %>%
#' @param cpus an integer specifying the number of cpus to use for the benchamrk. Default is 1
#' @param tasks a list which elements are object of class mlr::makeRegrTask()
#' @param learners a list which elements are object of class mlr::makeLearner()
#' @param measures a list of the mlr performance metrics you want to get. Default is list(rmse)
#' @param keep.pred a boolean specifying if you want to keep the bmr preds. defaut = FALSE
#' @param models a boolean specifying if you want to keep the bmr models. defaut = FALSE
#' @param grouping a numeric specifying the number of tasks you want to benchamrk in a single batch. Default to 1000
#' @param level a character specifying the paralelllization level. Default  = "mlr.benchmark"
#' @param resampling  a character specifying the type of mlr's CV. Default = LOO
#' @param path a character specifying the path where you want to save the bmr files.
#' @param prefix a character specifying the prefix you want to use for the bmr file names.
#' @return a list which elements are objects of class mlr::benchmark()

makeBatchOfBenchExp <- function(
  tasks,
  learners,
  measures = list(rmse),
  keep.pred = FALSE,
  models = FALSE,
  grouping = 1000,
  level = "mlr.benchmark",
  resamplings = "LOO",
  cpus = 1,
  prefix = "",
  path = "./"){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  bool = FALSE

  doBenchmark = function(){

    # set seed to make bmr experiments reproducibles
    set.seed(1985)

    # hack for tasks length when only a single task
    # ::todo::

    # split tasks in multiple subgroups to avoid memory saturation
    tasks.groups.start = seq(from = 1, to = length(tasks), by = grouping)
    tasks.groups.end = seq(from = grouping, to = length(tasks), by = grouping)

    # conducting the bmrs by subgroups
    lapply(seq_along(as.list(tasks.groups.start)),
      function(x) {

        # message
        message(paste0(
          "Conducting batch of benchmark experiments for tasks " ,
          tasks.groups.start[x], "-",
          tasks.groups.end[x]))

        # starting counting time of the current bmr execution
        tictoc::tic()

        # enable parallelization with level = mlr.resample
        if (cpus > 1) {
          parallelMap::parallelStart(mode = "multicore", cpus = cpus, level = level)
        }

        # hack to avoid wrong last task number
        if (is.na(tasks.groups.end[x])) {tasks.groups.end[x] = tasks.groups.start[x]}

        # benchmark
        bmr = mlr::benchmark(
          learners = learners,
          tasks = tasks[tasks.groups.start[x] :tasks.groups.end[x]],
          resamplings = mlr::makeResampleDesc(resamplings),
          measures = measures,
          keep.pred = keep.pred,
          models = models)

        # stop the parallelized computing
        if (cpus > 1) {
          parallelMap::parallelStop()
        }

        # stoping counting time
        exectime = tictoc::toc()
        exectime = exectime$toc - exectime$tic

        # save the bmr object to a file
        saveRDS(object = bmr, file = paste0(path,
          prefix, "_bmr_",  tasks.groups.start[x], "_", tasks.groups.end[x], ".rds"))

        # remove the object stored in RAM
        rm(bmr)

        # success message
        message(paste0(
          "Results of batch of Benchmark experiments for tasks " ,
          tasks.groups.start[x], "-",
          tasks.groups.end[x], "conducted and written to file. "))

        # loading all the temp bmr files and merging in a single big bmr object
        bmr_files = list.files(path = path, pattern = prefix, full.names = TRUE)
        bmrs = lapply(bmr_files, readRDS)

        if (length(bmrs) > 1) {bmrs = mergeBenchmarkResults(bmrs)}
        else {bmrs = bmrs[[1]]}

        # perfs + aggregated Performances
        perfs = getBMRPerformances(bmrs, as.df = TRUE)
        aggPerfs = getBMRAggrPerformances(bmrs, as.df = TRUE)
        summary = aggPerfs %>%
          dplyr::group_by(learner.id) %>%
          dplyr::select(rmse.test.rmse) %>%
          dplyr::summarise_all(
            funs(count = sum(!is.na(.)),
              min = min(.,na.rm = TRUE),
              max = max(.,na.rm = TRUE),
              mean = mean(.,na.rm = TRUE),
              median = median(.,na.rm = TRUE),
              sd = sd(.,na.rm = TRUE)))

        # return all the bmr results in a list
        return(benchmarked = list(
          perfs = perfs,
          aggPerfs = aggPerfs,
          summary = summary
        ))
      })
  }

  tryCatch(
    expr = {
      # add checks
      # ::TODO:

      # in case everything went fine, do makeBatchOfBenchExp
      output$value = doBenchmark()
      output$condition$type = "success"
      output$condition$message = "Dataset created"
      bool = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeBatchOfBenchExp raised a warning -> ",
        w)
      bool <<- TRUE
      output$value <<- doBenchmark()
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
      return(list(bool = bool, output = output))
    }
  )
}




