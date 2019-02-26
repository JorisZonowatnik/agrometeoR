#' @export
#' @title spatialize a gridded dataset using a mlr model
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param model an object of class mlr::train() that contains the prediction model
#' @param pred.grid an object of class sf::st_makegrid(). This object must contains the same column names as the task on which the model has been trained
#' @return a 2 elements named list : bool and output. bool is TRUE if function has provided the expected result. output is a named list which contains
#' (1) value : an object of class data.frame containing the spatialized data
#' (2) condition : a character specifying if the functions has encountered success, warning, error
#' (3) message : the message relative to the condition
#' @examples
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T15:00:00Z",
#'   sensor = "tsa")
#' myTask = makeTask(dataset = myDataset$output$value, target = "tsa")
#' myModel = makeModel(
#'   task = mytask$out$value,
#'   learner = learners$baseLearners$lrn.lm.alt)
#' mySpatialization = makeSpatialization(model = myModel$output$value)

makeSpatialization <- function(
  model,
  pred.grid = grid.df){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  bool = FALSE

  doMakeSpatialisation = function(){
    # predicting on the grid
    message("Predicting on grid...")

    # rename X and Y to x and y for mlr (gstat learner compatibility)
    pred.grid = pred.grid %>%
      dplyr::rename("x" = "X") %>%
      dplyr::rename("y" = "Y")

    pred = predict(model, newdata = pred.grid)
    spatialized = pred.grid %>%
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

    spatialized %>%
      dplyr::bind_cols(data.frame(coords))

    return(spatialized)
  }

  tryCatch(

    expr = {

      # check if model has proper class
      stopifnot(isTRUE("WrappedModel" %in% class(model)))

      # check if features used to build your model are present in your prediction grid
      stopifnot(isTRUE(all(toupper(model$features) %in% toupper(colnames(grid.df)))))

      # in case everything went fine, do MakeSpatialisation
      output$value = doMakeSpatialisation()
      output$condition$type = "success"
      output$condition$message = "Dataset created"
      bool = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeSpatialisation. raised a warning -> ",
        w)
      bool <<- TRUE
      output$value <<- doMakeSpatialisation()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::makeSpatialisation. raised an error -> ",
        e,
        "HINT 1 : check if model has proper class. ",
        "\n",
        "HINT 2 : check if features used to build your model are present in your prediction grid. ",
        "\n")
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "makeSpatialisation has encountered : ",
        output$condition$type,
        ". \n",
        "All done with makeSpatialisation. "
      )
      message(finalMessage)
      return(list(bool = bool, output = output))
    }
  )
}