#' @export
#' @title make a multicore parallelized mlr benchmark experiment for various learners on multiple tasks
#' @author Thomas Goossens
#' @import mlr
#' @param cores an integer specifying the number of cores to use for the benchamrk
#' @param tasks a list which elements are object of class mlr::makeRegrTask()
#' @param learners a list which elements are object of class mlr::makeLearner()
#' @param resampling  a character specifying the type of mlr's CV. Default = LOO
#' @return a list with an object of class mlr::benchmark()
makeBenchmark <- function(
  cores = 4,
  tasks,
  learners,
  resamplings = "LOO"){

  # enable parallelization
  # parallelStart(mode = "multicore", 4)

  # benchmark
  bmr = mlr::benchmark(
    learners = learners,
    tasks = tasks,
    resamplings = mlr::makeResampleDesc(resamplings),
    measures = list(rmse, mse, mae, timetrain))

  # perfs + aggregated Performances
  perfs = getBMRPerformances(bmr, as.df = TRUE)
  aggPerfs = getBMRAggrPerformances(bmr, as.df = TRUE)

  # create a list containing all the useful information
  benchmarkResults = list(
    bmr = bmr,
    perfs = perfs,
    aggPerfs = aggPerfs
    # summary = summary(m$learner.model)
  )

  # stop the parallelized computing
  # parallelStop()

  # return the model and its information contained in the list
  return(benchmarkResults)
}

