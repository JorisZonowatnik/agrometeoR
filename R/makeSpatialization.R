#' @export
#' @title spatialize to geojson (EPSG = 4326) a gridded dataset using a mlr model
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param model an object of class mlr::train() that contains the prediction model
#' @param grid an object of class sf::st_makegrid(). This object must contains the same column names as the task on which the model has been trained
#' @return a dataframe containing the spatialized data
makeSpatialization <- function(
  model,
  grid = grid.df){

  # rename X and Y to x and y for mlr (gstat learner compatibility)
  grid = grid %>%
    dplyr::rename("x" = "X") %>%
    dplyr::rename("y" = "Y")

  # predicting on the grid
  pred = predict(model, newdata = grid)
  spatialized = grid %>%
    dplyr::select(x, y, px) %>%
    dplyr::bind_cols(pred$data)

  # return the spatialized dataframe
  return(spatialized)
}

