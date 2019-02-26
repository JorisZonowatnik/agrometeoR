#' @export
#' @title make a mlr model + model metadata for a specific task using a specific learner
#' @author Thomas Goossens
#' @import mlr
#' @param task an object of class mlr::makeRegrTask()
#' @param learner an object of class mlr::makeLearner()
#' @return a 2 elements named list : bool and output. bool is TRUE if function has provided the expected result. output is a named list which contains :
#' (1) value : an object of class list made of 3 elements which are of classes "Model", "dataframe", and "list"
#' (2) condition : a character specifying if the functions has encountered success, warning, error
#' (3) message : the message relative to the condition
#' @examples
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T15:00:00Z",
#'   sensor = "tsa")
#' myTask = makeTask(dataset = myDataset$output$value, target = "tsa")
#' myModel = makeModel(
#'   task = mytask$out$value,
#'   learner = learners$baseLearners$lrn.lm.alt)

makeModel <- function(
  task,
  learner){

    output = list(value = NULL, condition = list(type = NULL, message = NULL))
    bool = FALSE

    doMakeModel = function(){
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
      predictions = predictions %>%
        dplyr::mutate(residuals = truth - response)

      # create a list containing all the useful information
      model = list(
        trained = trained,
        predictions = predictions,
        perfs = list(iters = perfs, agg = aggPerfs)
        # summary = summary(m$learner.model)
      )
      return(model)
    }

  tryCatch(

    expr = {

      # check if task has proper class
      stopifnot(length(which(class(task) %in% "RegrTask")) > 0)

      # check if learner has proper class
      stopifnot(length(which(class(learner) %in% "Learner")) > 0)

      # in case everything went fine, do makeModel
      output$value = doMakeModel()
      output$condition$type = "success"
      output$condition$message = "Dataset created"
      bool = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeModel raised a warning -> ",
        w)
      bool <<- TRUE
      output$value <<- doMakeModel()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::makeModel raised an error -> ",
        e)
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "makeModel has encountered : ",
        output$condition$type,
        ". \n",
        "All done with makeModel. "
      )
      message(finalMessage)
      return(list(bool = bool, output = output))
    }
  )
}

