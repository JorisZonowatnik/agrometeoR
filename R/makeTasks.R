#' @export
#' @title make one or multiple mlr regr task(s) for sets of hourly/daily records
#' @author Thomas Goossens
#' @param stations a character specifying the sid's of the stations to use separated by commas
#' @param dataset a dataframe containing all the hourly/daily records you want to transform to a list of mlr tasks
#' @param target a charchter specifying the name of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return a list containing a boolean and a list which elements are of class mlr::makeRegrTask()
makeTasks <- function(
  dataset,
  drop = NULL,
  target
){
  out = tryCatch({
    output = NULL
    bool = FALSE

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
      output = tasks

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
      message("AgrometeoR Error : makeTasks failed. Here is the original error message : ")
      message(paste0(cond, "\n"))
      message("Setting value of output to NA")
    },
    finally = {
      return(list(bool = bool, output = output))
    })
  return(out)
}


