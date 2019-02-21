#' @export
#' @title make one mlr regr task for a single set of hourly/daily records
#' @author Thomas Goossens
#' @param dataset a dataframe containing an hourly/daily set of records you want to transform to a mlr task
#' @param target a charachter specifying the name of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return a list containing a boolean and a list which single element is of class mlr::makeRegrTask()
makeTask <- function(
  dataset,
  drop = NULL,
  target
){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
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
        "Your dataset contains missing values at target variable. Stations with missing values are ignored for the task creation. ",
        "\n",
        "The sid of the stations used for the stations are : ",
        "\n",
        "***begin sid used stations***",
        "\n",
        paste0(dataset$sid, sep = ",", collapse = ""),
        "\n",
        "***end sid used stations***"
      ))
    }

    # in case everything went fine, do makeTask
    output$value = doMakeTask()
    output$condition$type = "success"
    output$condition$message = "Dataset created"
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
      output$condition$message <<- warning
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



