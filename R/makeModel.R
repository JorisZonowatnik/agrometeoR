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
   output = list(value = NULL, error = NULL)
   bool = FALSE

   if (!length(which(class(task) %in% "RegrTask")) > 0){
    stop("The argument task must have class 'RegrTask'. For more information, see mlr package documentation. ")
   }

   if (!length(which(class(learner) %in% "Learner")) > 0){
     stop("The argument learner must have class 'Learner'. For more information, see mlr package documentation. ")
   }

   withCallingHandlers({
     # message
     message("Training the learner to build a model...")

     # benchmark
     bmr = mlr::benchmark(
       learners = learner,
       tasks = task,
       resamplings = mlr::makeResampleDesc("LOO"),
       measures = list(rmse, mse, mae, timetrain),
       show.info = FALSE)

     # aggregated Performances
     perfs = getBMRPerformances(bmr, as.df = TRUE)
     aggPerfs = getBMRAggrPerformances(bmr, as.df = TRUE)

     # training the learner to create the model
     trained = mlr::train(
       learner = learner,
       task = task)

     # creating the residuals + predictions at stations dataframe
     predictions = data.frame(predict(trained, newdata = getTaskData(task))$data)
     residuals = predictions %>%
       dplyr::mutate(residuals = truth - response) %>%
       dplyr::select(residuals)


     # create a list containing all the useful information
     model = list(
       trained = trained,
       stations_pred = predictions,
       perfs = list(iters = perfs, agg = aggPerfs),
       residuals = residuals
       # summary = summary(m$learner.model)
     )

     # storing to output
     output$value = model

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
     error = paste0(
       "AgrometeoR Error : makeModel failed. Here is the original error message : ",
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



