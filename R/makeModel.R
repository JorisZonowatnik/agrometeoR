#' @export
#' @title make a mlr model + model metadata for a specific task using a specific learner
#' @author Thomas Goossens
#' @import mlr
#' @param task an object of class mlr::makeRegrTask()
#' @param learner an object of class mlr::makeLearner()
#' @return a list containing a boolean and an object of class mlr::train()
makeModel <- function(
  task,
  learner){
 out = tryCatch({
   output = NA
   bool = FALSE

   withCallingHandlers({
     message("Training the learner to build a model...")

     # benchmark
     bmr = mlr::benchmark(
       learners = learner,
       tasks = task,
       resamplings = mlr::makeResampleDesc("LOO"),
       measures = list(rmse, mse, mae, timetrain))

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
       trained = m,
       stations_pred = predictions,
       perfs = list(iters = perfs, agg = aggPerfs),
       residuals = residuals
       # summary = summary(m$learner.model)
     )

     output = modelInfo

     # success message
     message("Success ! Model created")
     bool = TRUE
   },
   warning = function(cond){
     message("AgrometeoR Warning :")
     message(cond)
   })
 },
   error = function(cond){
     message("AgrometeoR Error : makeSpatialization failed. Here is the original error message : ")
     message(paste0(cond, "\n"))
     message("Setting value of output to NA")
   },
   finally = {
     return(list(bool = bool, output = output))
   })
 return(out)
}



