#' @export
#' @title spatialize a gridded dataset using a mlr model
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param model an object of class mlr::train() that contains the prediction model
#' @param grid an object of class sf::st_makegrid(). This object must contains the same column names as the task on which the model has been trained
#' @return a list containing a boolean and a dataframe containing the spatialized data
makeSpatialization <- function(
  model,
  grid = grid.df){

  out = tryCatch({
    output = list(value = NULL, error = NULL)
    bool = FALSE

    if (!isTRUE(class(model) == "WrappedModel")) {
      stop("Provided model must be of class WrappedModel")
    }

    if (!isTRUE(all(toupper(model$features) %in% toupper(colnames(grid.df))))) {
      stop("The features used to build your model are not present in your prediction grid")
    }

    withCallingHandlers({
      # rename X and Y to x and y for mlr (gstat learner compatibility)
      grid = grid %>%
        dplyr::rename("x" = "X") %>%
        dplyr::rename("y" = "Y")

      # predicting on the grid
      message("Predicting on grid...")

      pred = predict(model, newdata = grid)
      spatialized = grid %>%
        dplyr::select(x, y, px) %>%
        dplyr::bind_cols(pred$data)

      # convert to spatial object to change CRS
      spatialized = spatialized %>%
        sf::st_as_sf(coords = c("x","y"))

      # set crs
      spatialized = spatialized %>%
        sf::st_set_crs(3812)

      # convert to CRS = 4326 (geojson standard)
      spatialized = spatialized %>%
        sf::st_transform(4326)

      # making it df again with x and y cols
      coords = sf::st_coordinates(spatialized)
      sf::st_geometry(spatialized) = NULL
      output$value = spatialized %>%
        dplyr::bind_cols(data.frame(coords))

      # success message and boolean
      message("Success ! Data spatialized")
      bool = TRUE
    },
    warning = function(cond){
      message("AgrometeoR Warning :")
      message(cond)
    })
  },
  error = function(cond){
    error = paste0(
      "AgrometeoR Error : makeSpatialization failed. Here is the original error message : ",
      cond,
      "\n",
      "value of output set to NULL")
    output$error = error
    message(error)
  },
  finally = {
    return(list(bool = bool, output = output))
  })
  return(out)
}

