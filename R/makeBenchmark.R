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
#' @return a list wihch elements are objects of class mlr::benchmark()
makeBenchmark <- function(
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

  out = tryCatch({
    output = list(value = NULL, error = NULL)
    bool = FALSE

    withCallingHandlers({

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
            "Conducting Benchmark for tasks " ,
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

          # success message and boolean
          message(paste0(
            "Success ! Benchmark for tasks " ,
            tasks.groups.start[x], "-",
            tasks.groups.end[x], "conducted and written to file"))
        })

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

        # create a list containing all the useful information
        output$value = list(
          bmr = bmrs,
          perfs = perfs,
          aggPerfs = aggPerfs,
          summary = summary
        )

        bool = TRUE

    },
      warning = function(cond){
        message("AgrometeoR Warning :")
        message(cond)
      })
  },
    error = function(cond){
      error = paste0(
        "AgrometeoR Error : makeBenchmark failed. Here is the original error message : ",
        cond,
        "\n",
        "value of output set to NULL")
      output$error = error
      message(error)
    },
    finally = {
      return(list(bool = bool, output = output))
    })
  return(out)

}



