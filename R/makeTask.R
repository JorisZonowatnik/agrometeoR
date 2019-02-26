#' @export
#' @title make one mlr regr task for a single set of hourly/daily records
#' @author Thomas Goossens
#' @param dataset a dataframe containing an hourly/daily set of records you want to transform to a mlr task
#' @param target a charachter specifying the name of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return a 2 elements named list : (1) snitch and (2) output. snitch is TRUE if function has provided the expected result. output is a named list which contains 4 elements :
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

  output = list(value = NULL, condition = list(type = NULL, message = NULL), stations = list(used = NULL, ignored = NULL))
  snitch = FALSE

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

    # compute a summary for both response and standard error
    summary = data.frame(do.call(cbind, lapply(mlr::getTaskData(task), summary)))

    return(list(task = task, summary = summary))

    return(task)
  }

   tryCatch(

    expr = {

    # check if dataset has class data.frame
    stopifnot(class(dataset) == "data.frame")

    # check if target exists in dataframe
    stopifnot(target %in% colnames(dataset))

    # check if missing values in target or features
    if (!identical(na.omit(dataset), dataset)) {

      # store the sids of the stations with missing data
      output$stations$ignored = as.integer(setdiff(dataset$sid, na.omit(dataset)$sid))

      # remove stations with missing data from the dataset
      dataset = na.omit(dataset)

      # raise warning telling dataset has been amputed of stations with missing values
      warning(paste(
        "Your dataset contains missing values either at target variable and/or features. Stations with missing values are ignored for the task creation. ",
        "The sid of the ignored stations are : ",
        paste0(output$stations$ignored, sep = ",", collapse = "")
      ))
    }

    # in case everything went fine, do makeTask
    output$value = doMakeTask()
    output$stations$used = as.integer(dataset$sid)
    output$condition$type = "success"
    output$condition$message = "Dataset created"
    snitch = TRUE

    },
    # in case of warning, store the warning and do makeTask
    warning = function(w){

      warning = paste0(
        "AgrometeoR::makeTask raised a warning -> ",
        w)
      output$value <<- doMakeTask()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
      output$stations$used <<- as.integer(dataset$sid)
      snitch <<- TRUE
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
        "makeTask has encountered : ",
        output$condition$type,
        ". \n",
        "All done with makeTask. "
      )
      message(finalMessage)
      return(list(snitch = snitch, output = output))
    }
   )
}




