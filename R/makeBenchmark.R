#' @export
#' @title make a mlr benchmark experiment for various learners on multiple tasks
#' @author Thomas Goossens
#' @import mlr
#' @param tasks a list which elements are object of class mlr::makeRegrTask()
#' @param learners a list which elements are object of class mlr::makeLearner()
#' @return an object of class mlr::makebenchmark()
makeBenchmark <- function(
  tasks,
  learners){

  # benchmark
  bmr = mlr::benchmark(
    learners = learner,
    tasks = task,
    resamplings = mlr::makeResampleDesc("LOO"),
    measures = list(rmse, mse, mae, timetrain, expvar))

  # aggregated Performances
  perfs = getBMRPerformances(bmr, as.df = TRUE)
  aggPerfs = getBMRAggrPerformances(bmr, as.df = TRUE)

  # training the learner to create the model
  m = mlr::train(
    learner = learner,
    task = task)

  # creating the residuals + predictions at stations dataframe
  residuals = data.frame(residuals(m$learner.model))
  colnames(residuals) = "residuals"
  predictions = data.frame(predict(m$learner.model))
  colnames(predictions) = "stations_pred"

  # create a list containing all the useful information
  modelInfo = list(
    model = m,
    stations_pred = predictions,
    perfs = list(iters = perfs, agg = aggPerfs),
    residuals = residuals
    # summary = summary(m$learner.model)
  )

  # return the model and its information contained in the list
  return(modelInfo)
}

