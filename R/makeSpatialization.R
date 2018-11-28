#' @export
#' @title spatialize to geojson (EPSG = 4326) a gridded dataset using a mlr model
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param model an object of class mlr::train() that contains the prediction model
#' @param grid an object of class sf::st_makegrid(). This object must contains the same column names as the task on which the model has been trained
#' @param file a boolean specifying wether the saptiliazed dat should be written as a geojson file or simply output into the console.
#' @param path a character specifying the path where you want your geosonfile to be stored. Default = wd
#' @param filename a character specifying the name you want to give to the file.
#' @return a geojson object containing the spatialized data
makeSpatialization <- function(
  model,
  grid = grid.df,
  file = FALSE,
  filename = NULL,
  path = getwd()){

  # rename X and Y to x and y for mlr (gstat learner compatibility)
  grid = grid %>%
    dplyr::rename("x" = "X") %>%
    dplyr::rename("y" = "Y")

  # predicting on the grid
  pred = predict(model, newdata = grid)
  pred_grid = grid %>%
    dplyr::select(x,y) %>%
    dplyr::bind_cols(pred$data)

  # convert to spatial object
  pred_grid = pred_grid %>%
    sf::st_as_sf(coords = c("x","y"))

  # set crs
  pred_grid = pred_grid %>%
    sf::st_set_crs(3812)

  # convert to CRS = 4326 (geojson standard)
  pred_grid = pred_grid %>%
    sf::st_transform(4326)

  if (isTRUE(file)) {
    # exporting to geojson
    sf::st_write(obj = pred_grid, dsn = paste0(path, "/", filename, ".geojson"))
  } else {
    geojson = geojsonio::geojson_json(pred_grid)
    cat(geojson)
  }

  # reading the geojson
  # cat(readLines('spatialized.geojson'), sep = '\n')

  # deleting the temporary geojson file
  # file.remove("spatialized.geojson")
}

