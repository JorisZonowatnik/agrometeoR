#' @export
#' @title make a mlr model + its metadata for a specific task using a specific learner
#' @author Thomas Goossens
#' @import mlr
#' @param task an object of class mlr::makeRegrTask()
#' @param learner an object of class mlr::makeLearner()
#' @return A 2 elements named list
#' \itemize{
#'   \item \code{snitch} : a boolean. Is \code{TRUE} if function has provided the expected result. Is \code{FALSE} is function throws an error
#'   \item \code{output} : a named list which elements are :\itemize{
#'     \item \code{value} : a named list which elements are : \itemize{
#'       \item \code{trained} : an element of class \code{mlr::makeWrappedMode()}
#'       \item \code{predictions} an element of class \code{data.frame} containing the predictions made by the trained model at station locations. Colnames are \code{truth} (= observations), \code{response} (= predictions), \code{se} (= standard error) and \code{residuals} (= truth - response)
#'       \item \code{perfs} : a named list which elements are : \itemize{
#'         \item \code{iters} : a dataframe containing the information relative to the leave-one-out cross validation of the model. Colnames are \code{learner.id}, \code{iter}, \code{rmse}, \code{mse}, \code{mae}, \code{timetrain}
#'         \item \code{agg} : a dataframe containing the aggregated performances of the LOOCV. Same colnames as \code{iters}
#'         }
#'      }
#'       \item \code{condition} : a character specifying the condition encountered by the : success, warning, or error.
#'       \item \code{message} : a character specifying the message relative to the condition.
#'     }
#'  }
#' @examples
#'\dontrun{
#' # create the dataset
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T18:00:00Z",
#'   sensor = "tsa")
#'
#' # extract the list of hourly sets of records
#' myDataset = myDataset$output$value
#'
#' # create the tasks
#' myTasks = purrr::map(myDataset, makeTask, target = "tsa")
#'
#' # extract the tasks from the outputs
#' myTasks = myTasks %>% purrr::modify_depth(1, ~.$"output"$"value"$"task")
#'
#' # keep the first task
#' myTask = myTasks[[1]]
#'
#' # create the model
#' myModel = makeModel(
#'   task = myTask,
#'   learner = agrometeorLearners$mulLR_lonLatAlt_NA)
#'
#' # extract the relevant information
#' myModel = myModel$output$value
#'
#' # show the results of the LOOCV of this hour forthe choosen learner
#' head(myModel$predictions)
#' head(myModel$perfs$iters)
#'
#' # show the model information
#' myModel$trained
#' }

makeModel <- function(
  task,
  learner){

    output = list(value = NULL, condition = list(type = NULL, message = NULL))
    snitch = FALSE

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
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeModel raised a warning -> ",
        w)
      snitch <<- TRUE
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
      return(list(snitch = snitch, output = output))
    }
  )
}

