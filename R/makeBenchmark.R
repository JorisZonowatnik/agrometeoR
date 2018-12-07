#' @export
#' @title make a multicore parallelized mlr benchmark experiment for various learners on multiple tasks
#' @author Thomas Goossens
#' @import mlr
#' @param cores an integer specifying the number of cores to use for the benchamrk
#' @param tasks a list which elements are object of class mlr::makeRegrTask()
#' @param learners a list which elements are object of class mlr::makeLearner()
#' @param resampling  a character specifying the type of mlr's CV. Default = LOO
#' @return a list wihch elements are objects of class mlr::benchmark()
makeBenchmark <- function(
  parallel = TRUE,
  cores = 4,
  tasks,
  learners,
  resamplings = "LOO"){

  out = tryCatch({
    output = list(value = NULL, error = NULL)
    bool = FALSE

    withCallingHandlers({

      # message
      message("Running benchmark...")

      # enable parallelization with level = mlr.resample
      if (isTRUE(parallel)) {
        parallelStart(mode = "multicore", cpus = cores, level = "mlr.resample")
      }

      # benchmark
      bmr = mlr::benchmark(
        learners = learners,
        tasks = tasks,
        resamplings = mlr::makeResampleDesc(resamplings),
        measures = list(rmse, mse, mae, timetrain))

      # stop the parallelized computing
      if (isTRUE(parallel)) {
        parallelStop()
      }

      # perfs + aggregated Performances
      perfs = getBMRPerformances(bmr, as.df = TRUE)
      aggPerfs = getBMRAggrPerformances(bmr, as.df = TRUE)

      # create a list containing all the useful information
      output$value = list(
        bmr = bmr,
        perfs = perfs,
        aggPerfs = aggPerfs
        # summary = summary(m$learner.model)
      )

      # success message and boolean
      message("Success ! Benchmark conducted")
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



