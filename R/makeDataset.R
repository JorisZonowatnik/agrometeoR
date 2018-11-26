#' @export
#' @title make a dataframe of stations records
#' @author Thomas Goossens
#' @param stations a character specifying the sid's of the stations to use separated by commas
#' @param dfrom a datetime string specifying the dateTime
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param dto a datetime string specifying the dateTime
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param sensor a character specifying the sensor data you want to spatialize.
#' One of tsa, hct, hra
#' @param dynExpl a character vector specifying the dynamic explanatory variables you want to add to the task.
#' Any combinations of inca, ens
#' @param staticExpl a character vector specifying the static explanatory variables you want to add to the taks.
#' Any combinations of "X", "Y", altitude", "elevation", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest", "Herbaceous_vegetation"
#' @return an object of class dataframe containing the desired records
makeDataset <- function(
  stations = paste0(as.character(stations.df$sid), collapse = ","),
  dfrom,
  dto,
  sensor = "tsa",
  staticExpl = c("X", "Y", "elevation"),
  dynExpl = NULL
){

  # make an API call to retrieve the dynamic data
  dataset = typeData(
    getData(dfrom = dfrom, dto = dto, sensors = sensor, sid = stations )
  )

  # Keep only the relevant columns
  dataset = dataset %>%
    dplyr::select("sid", "mtime", sensor)

  # join with static explanatory vars
  dataset = dataset %>%
    dplyr::left_join(
      stations.df %>%
        dplyr::select(c("sid", staticExpl)),
      by = "sid")

  # rename X and Y to x and y for mlr (gstat learner compatibility)
  dataset = dataset %>%
    dplyr::rename("y" = "Y") %>%
    dplyr::rename("x" = "X")

  # return data
  return(dataset)

}