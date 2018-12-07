#' @export
#' @title make one or multiple mlr regr task(s) for sets of hourly/daily records
#' @author Thomas Goossens
#' @param dataset a dataframe containing all the hourly/daily records you want to transform to a list of mlr tasks
#' @param target a charachter specifying the name of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return a list containing a boolean and a list which elements are of class mlr::makeRegrTask()
makeTasks <- function(
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
      dataset = dataset %>%
        dplyr::mutate(task.id = gsub("[^[:digit:]]", "", mtime)) %>%
        dplyr::select(-mtime)

      # group by task.id and make lists of dataframes
      dataset = split(
        x = dataset,
        f = dataset$task.id)

      # make machine learning regression tasks for each list element
      tasks = lapply(seq_along(dataset), function(d)
        mlr::dropFeatures(
          task = mlr::makeRegrTask(
            data = dataset[[d]] %>% dplyr::select(-c(task.id, sid)),
            target = target,
            id = names(dataset)[d]),
          features = drop)
      )
      # naming tasks
      names(tasks) = names(dataset)
      output$value = tasks

      # success message and boolean
      message("Success ! Task(s) created ")
      bool = TRUE
    },
      warning = function(cond){
        message("AgrometeoR Warning :")
        message(cond)
      })
  },
    error = function(cond){
      error = paste0(
        "AgrometeoR Error : makeTasks failed. Here is the original error message : ",
        cond,
        "value of output set to NULL")
      message(error)
      output$error = error
    },
    finally = {
      return(list(bool = bool, output = output))
    })
  return(out)
}


