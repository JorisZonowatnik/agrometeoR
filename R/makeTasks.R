#' @export
#' @title make one or multiple mlr regr task(s) for sets of hourly/daily records
#' @author Thomas Goossens
#' @param stations a character specifying the sid's of the stations to use separated by commas
#' @param dataset a dataframe containing all the hourly/daily records you want to transform to a list of mlr tasks
#' @param target a charchter specifying the name of the target variable
#' @param drop a character vector specifying the explanatory variables you wan to drop.
#' @return an object of class list containing elements of class mlr::makeRegrTask()
makeTasks <- function(
  dataset,
  drop = NULL,
  target
){

  # dataset renaming mtime vars for list grouping
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

  names(tasks) = names(dataset)

  # summarise the dataset
  # insights = summary(mlr::getTaskData(task))

  # return the list containing the task and its insights
  return(tasks)

}
