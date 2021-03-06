#' @export
#' @title make a spatialization of a gridded dataset using a mlr model
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param model an object of class mlr::train() that contains the prediction model
#' @param pred.grid an object of class sf::st_makegrid(). This object must contains the same column names as the task on which the model has been trained
#' @return A 2 elements named list
#' \itemize{
#'   \item \code{snitch} : a boolean. Is \code{TRUE} if function has provided the expected result. Is \code{FALSE} is function throws an error
#'   \item \code{output} : a named list which elements are :\itemize{
#'     \item \code{value} : a named list which elements are : \itemize{
#'       \item \code{spatialized} : an element of class \code{data.frame}. colnames are \code{px} (= reference of the pixel), \code{response} (= prediction value), \code{se} (= prediction standard error)
#'       \item \code{summary} an element of class \code{data.frame} containing summary information about grid prediction. colnames are \code{min.response}, \code{max.response}, \code{mean.response}, \code{min.se}, \code{max.se}, \code{mean.se}
#'      }
#'       \item \code{condition} : a character specifying the condition encountered by the function: success, warning, or error.
#'       \item \code{message} : a character specifying the message relative to the condition.
#'     }
#'  }
#' @examples
#'\dontrun{
#' # load magrittr for pipe use : %>%
#' library(magrittr)
#' # create the dataset
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T18:00:00Z",
#'   sensor = "tsa")
#'
#' # extract the list of hourly sets of records
#' myDataset = myDataset$output$value
#'
#' # create the tasks
#' myTasks = purrr::map(myDataset, makeTask, target = "tsa")
#'
#' # extract the tasks from the outputs
#' myTasks = myTasks %>% purrr::modify_depth(1, ~.$"output"$"value"$"task")
#'
#' # keep the first task
#' myTask = myTasks[[1]]
#'
#' # create the model
#' myModel = makeModel(
#'   task = myTask,
#'   learner = agrometeorLearners$mulLR_lonLatAlt_NA)
#'
#' # extract the relevant information
#' myModel = myModel$output$value
#'
#' # spatialize using the trained model
#' mySpatialization = makeSpatialization(
#' model = myModel$trained,
#' pred.grid = grid.df) # grid.df comes precompiled with the package
#'
#' # get the relevant information
#' mySpatialization = mySpatialization$output$value
#'
#' # show an excerpt of the spatialized data
#' head(mySpatialization$spatialized)
#'
#' # show the summary stats of spatialized data
#' head(mySpatialization$summary)
#' }

makeSpatialization <- function(
  model,
  pred.grid = grid.df){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

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

    # compute a summary for both response and standard error

    summary = (data.frame(
      min.response = min(spatialized[["response"]]),
      max.response = max(spatialized[["response"]]),
      mean.response = mean(spatialized[["response"]]),

      min.se = min(spatialized[["se"]]),
      max.se = max(spatialized[["se"]]),
      mean.se = mean(spatialized[["se"]])
    ))

    return(list(spatialized = spatialized, summary = summary))
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
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeSpatialisation. raised a warning -> ",
        w)
      snitch <<- TRUE
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
      return(list(snitch = snitch, output = output))
    }
  )
}
