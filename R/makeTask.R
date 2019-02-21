#' @export
#' @title make one mlr regr task for a single set of hourly/daily records
#' @author Thomas Goossens
#' @param dataset a dataframe containing an hourly/daily set of records you want to transform to a mlr task
#' @param target a charachter specifying the name of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return a list containing a boolean and another list.
#' The later contains 4 elements :
#' (1) value : an object which classes are "RegrTask" "SupervisedTask" "Task"
#' (2) condition : a character specifying if the functions has encountered success, warning, error
#' (3) message : the message relative to the condition
#' (4) stations : a numeric vector containing the sids of the used stations
#' @examples
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T15:00:00Z",
#'   sensor = "tsa")
#' myTask = makeTask(dataset = myDataset$output$value, target = "tsa")

makeTask <- function(
  dataset,
  drop = NULL,
  target
){

  output = list(value = NULL, condition = list(type = NULL, message = NULL), stations = NULL)
  bool = FALSE

  doMakeTask = function(){
    message("Making mlr task(s)...")

    # creating the id of the task
    task.id = gsub("[^[:digit:]]", "", unique(dataset$mtime))

    # removing the mtime & sid column as not an explanatory information
    dataset = dataset %>%
      dplyr::select(-c(mtime, sid))

    # make a machine learning regression task
    task = mlr::dropFeatures(
      task = mlr::makeRegrTask(
        data = dataset,
        target = target,
        id = task.id),
      features = drop)

    return(task)
  }

   tryCatch(

    expr = {

    # check if dataset has class data.frame
    stopifnot(class(dataset) == "data.frame")

    # check if target exists in dataframe
    stopifnot(target %in% colnames(dataset))

    # check if missing values in target or features and raise warning telling dataset has been amputed of stations with missing values if its the case
    if (!identical(na.omit(dataset), dataset)) {
      dataset = na.omit(dataset)

      warning(paste(
        "Your dataset contains missing values either at target variable and/or features. Stations with missing values are ignored for the task creation. ",
        "The sid of the stations used for the stations are : ",
        paste0(dataset$sid, sep = ",", collapse = "")
      ))
    }

    # in case everything went fine, do makeTask
    output$value = doMakeTask()
    output$condition$type = "success"
    output$condition$message = "Dataset created"
    output$stations = dataset$sid
    bool = TRUE

    },
    # in case of warning, raise a warning and do makeTask
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeTask raised a warning -> ",
        w)
      bool <<- TRUE
      output$value <<- doMakeTask()
      output$condition$type <<- "warning"
      output$condition$message <<- w$message
      output$stations <<- dataset$sid
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::makeTask raised an error -> ",
        e)
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {

      finalMessage = paste0(
        "makeTask has encountered a condition of type : ",
        output$condition$type,
        ". \n",
        "All done with makeTask. "
      )
      message(finalMessage)
      return(list(bool = bool, output = output))
    }
   )
}




