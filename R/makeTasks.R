#' @export
#' @title make one or multiple mlr regr task(s) for sets of hourly/daily records
#' @author Thomas Goossens
#' @param stations a character specifying the sid's of the stations to use separated by commas
#' @param dataset a dataframe containing all the hourly/daily records you want to transform to a list of mlr tasks
#' @param target a charchter specifying the neame of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return an object of class list containing elements of class mlr::makeRegrTask()
makeTasks <- function(
  dataset,
  drop = c("sid", "mtime"),
  target
){

  # group by mtime and make lists of dataframes
  dataset = split(dataset, dataset$mtime)

  # make machine learning regression tasks for each list element
  tasks = lapply(seq_along(dataset), function(d)
    mlr::dropFeatures(
      task = mlr::makeRegrTask(
        data = dataset[[d]] %>%
          dplyr::mutate_at("mtime", as.numeric),
        target = target,
        id = gsub(":", "", names(dataset)[d])),
      features = drop)
    )
  names(tasks) = gsub(":", "", names(dataset))

  # summarise the dataset
  # insights = summary(mlr::getTaskData(task))

  # return the list containing the task and its insights
  return(tasks)

}
