#' @export
#' @title make one mlr regr task(s) for a single set of hourly/daily records
#' @author Thomas Goossens
#' @param dataset a dataframe containing an hourly/daily set of records you want to transform to a mlr task
#' @param target a charachter specifying the name of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return a list containing a boolean and a list which elements are of class mlr::makeRegrTask()
makeTask <- function(
  dataset,
  drop = NULL,
  target
){
  out = tryCatch({
    output = list(value = NULL, error = NULL)
    bool = FALSE

    if (!class(dataset) == "data.frame") {
      stop(paste(
        "The argument dataset must have class 'data.frame'. ",
        "\n"
      ))
    }

    if (!target %in% colnames(dataset)) {
      stop(paste(
        "Your dataset does not contain a variable that matches your target argument "
        , target,
        "\n"
      ))
    }

    withCallingHandlers({

      # dataset renaming mtime vars for list grouping
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

      # assigning the task to the output value
      output$value = task

      # success message and boolean
      message("Success ! Task created ")
      bool = TRUE
    },
      warning = function(cond){
        message("AgrometeoR Warning :")
        message(cond)
      })
  },
    error = function(cond){
      error = paste0(
        "AgrometeoR Error : makeTask failed. Here is the original error message : ",
        cond,
        "Value of output set to NULL.")
      message(error)
      output$error = error
    },
    finally = {
      return(list(bool = bool, output = output))
    })
  return(out)
}


