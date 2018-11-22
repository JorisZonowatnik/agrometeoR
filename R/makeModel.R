#' @export
#' @title make a mlr model + model metadata for a specific task using a specific learner
#' @author Thomas Goossens
#' @import mlr
#' @param task an object of class mlr::makeRegrTask()
#' @param learner an object of class mlr::makeLearner()
#' @return an object of class mlr::train()
makeModel <- function(
  task,
  learner){

  # benchmark
  bmr = mlr::benchmark(
    learners = learner,
    tasks = task,
    resamplings = mlr::makeResampleDesc("LOO"),
    measures = list(rmse, mse, mae, timetrain, expvar))

  # aggregated Performances
  perfs = getBMRAggrPerformances(bmr, as.df = TRUE)

  # training the learner to create the model
  m = mlr::train(
    learner = learner,
    task = task)

  # creating the residuals + predictions at stations dataframe
  # residuals = data.frame(residuals(m$learner.model))
  # colnames(residuals) = "residuals"
  predictions = data.frame(predict(m$learner.model))
  colnames(predictions) = "stations_pred"

  # create a list containing all the useful information
  modelInfo = list(
    model = m,
    stations_pred = predictions
    # residuals = residuals,
    # perfs = perfs,
    # summary = summary(m$learner.model)
  )

  # return the model and its information contained in the list
  return(modelInfo)
}

