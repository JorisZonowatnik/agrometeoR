#' @export
#' @title make a mlr regr task for an hourly/daily set of records
#' @author Thomas Goossens
#' @param stations a character specifying the sid's of the stations to use separated by commas
#' @param dataset a dataframe containing all the hourly/daily records you want to transform a list of mlr tasks
#' @param dynExpl a character vector specifying the dynamic explanatory variables you want to add to the task.
#' Any combinations of inca, ens
#' @param staticExpl a character vector specifying the static explanatory variables you want to add to the taks.
#' Any combinations of "X", "Y", altitude", "elevation", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest", "Herbaceous_vegetation"
#' @return an object of class list containing elements of class mlr::makeRegrTask()
makeTasks <- function(
  dataset,
  staticExpl,
  dynExpl
){

  # remove useless columns
  dataset = dataset %>%
    dplyr::select(c("sid", sensor, "mtime"))

  # join with explanatory vars
  dataset = dataset %>%
    dplyr::left_join(
      stations.df %>%
        dplyr::select(staticExpl),
      by = "sid")

  # rename X and Y to x and y for mlr (gstat learner compatibility)
  data = data %>%
    dplyr::rename("x" = "X") %>%
    dplyr::rename("y" = "Y")

  # group by mtime and make lists of dataframes
  data = split(data, data$mtime)

  # make machine learning regression tasks for each list element
  tasks = lapply(seq_along(data), function(d)
    mlr::dropFeatures(
      task = mlr::makeRegrTask(
        data = dplyr::select(data[[d]], -mtime),
        target = sensor,
        id = gsub(":", "", names(data)[d])),
      features = drop)
    )
  names(tasks) = gsub(":", "", names(data))

  # summarise the data
  # insights = summary(mlr::getTaskData(task))

  # return the list containing the task and its insights
  return(tasks)

}
