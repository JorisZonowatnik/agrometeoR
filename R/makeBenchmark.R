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
#' @param resampling  a character specifying the type of mlr's CV. Default = LOO
#' @return a list wihch elements are objects of class mlr::benchmark()
makeBenchmark <- function(
  tasks,
  learners,
  measures = list(rmse),
  keep.pred = FALSE,
  models = FALSE,
  resamplings = "LOO",
  cpus = 1){

  out = tryCatch({
    output = list(value = NULL, error = NULL)
    bool = FALSE

    withCallingHandlers({

      # message
      message("Running benchmark...")

      # starting counting time of the bmr execution
      tictoc::tic()

      # enable parallelization with level = mlr.resample
      if (cpus > 1) {
        parallelMap::parallelStart(mode = "multicore", cpus = cpus, level = "mlr.resample")
      }

      # set seed to make bmr experiments reproducibles
      set.seed(1985)

      # benchmark
      bmr = mlr::benchmark(
        learners = learners,
        tasks = tasks,
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

      # perfs + aggregated Performances
      perfs = getBMRPerformances(bmr, as.df = TRUE)
      aggPerfs = getBMRAggrPerformances(bmr, as.df = TRUE)
      summary = aggPerfs %>%
        dplyr::group_by(learner.id) %>%
        dplyr::summarise_at(
          .vars = 'rmse.test.rmse',
          .funs = c(min, max, mean, median, sd))
      colnames(summary) = c("min", "max", "mean", "median", "sd")

      # create a list containing all the useful information
      output$value = list(
        bmr = bmr,
        perfs = perfs,
        aggPerfs = aggPerfs,
        summary = summary,
        exectime = exectime
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



