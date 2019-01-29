#' @export
#' @title make a multicore parallelized mlr benchmark experiment for various learners on multiple tasks
#' @author Thomas Goossens
#' @import mlr
#' @import parallelMap
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
  path = "./"){

  out = tryCatch({
    output = list(value = NULL, error = NULL)
    bool = FALSE

    withCallingHandlers({

      # set seed to make bmr experiments reproducibles
      set.seed(1985)

      # split tasks in multiple subgroups if length > 1000 to avoid memory saturation
      tasks.groups = seq(from = 1, to = length(tasks), by = grouping)

      # conducting the bmrs by subgroups
      lapply(seq_along(as.list(tasks.groups)),
        function(x) {

          # message
          message(paste0(
            "Conducting Benchmark for tasks " ,
            tasks.groups[x], "-",
            tasks.groups[x+1]))

          # starting counting time of the current bmr execution
          tictoc::tic()

          # enable parallelization with level = mlr.resample
          if (cpus > 1) {
            parallelMap::parallelStart(mode = "multicore", cpus = cpus, level = level)
          }

          # hack to avoid wrong last task number
          if (is.na(tasks.groups[x+1])) {tasks.groups[x+1] = length(tasks)}

          # hack to avoid repeated benchamrkin while jumping to other set of 1000 tasks
          # defining next task
          x.next = x+1
          if (x %in% seq(from = 1, to = length(tasks), by = grouping)) { x.next == x}

          # benchmark
          bmr = mlr::benchmark(
            learners = learners,
            tasks = tasks[tasks.groups[x]:tasks.groups[x.next]],
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
            "bmr.", tasks.groups[x], "-", tasks.groups[x.next], ".rds"))

          # remove the object stored in RAM
          rm(bmr)

          # success message and boolean
          message(paste0(
            "Success ! Benchmark for tasks " ,
            tasks.groups[x], "-",
            tasks.groups[x.next], "conducted and written to file"))
        })

        # loading all the temp bmr files and merging in a single big bmr object
        bmr_files = list.files(path = path, pattern = ".rds", full.names = TRUE)
        bmrs = lapply(bmr_files, readRDS)
        big_bmr = mergeBenchmarkResults(bmrs)

        # perfs + aggregated Performances
        perfs = getBMRPerformances(big_bmr, as.df = TRUE)
        aggPerfs = getBMRAggrPerformances(big_bmr, as.df = TRUE)
        summary = aggPerfs %>%
          dplyr::group_by(learner.id) %>%
          dplyr::summarise_at(
            .vars = 'rmse.test.rmse',
            .funs = c(min, max, mean, median, sd))
        colnames(summary) = c("min", "max", "mean", "median", "sd")

        # create a list containing all the useful information
        output$value = list(
          bmr = big_bmr,
          perfs = perfs,
          aggPerfs = aggPerfs,
          summary = summary,
          exectime = exectime
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



