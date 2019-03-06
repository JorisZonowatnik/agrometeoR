#' @export
#' @title make a dataframe of stations records
#' @author Thomas Goossens
#' @param token a character specifying your agrometAPI token
#' @param stations a character vector specifying the sid's of the stations to use
#' @param json a character specifying the path of a json file constructed from a batch db export.
#' If NULL the API will be called.
#' @param dfrom a datetime string specifying the dateTime
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param dto a datetime string specifying the dateTime
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param sensor a character vector specifying the sensor data you want to spatialize.
#' One of tsa, hct, hra, ens, plu, vvt, sunrise, sunset
#' @param dynExpl a character vector specifying the dynamic explanatory variables you want to add to the task.
#' Any combinations of inca, ens
#' @param staticExpl a character vector specifying the desired static explanatory variables
#' Any combinations of "altitude", "elevation", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest", "Herbaceous_vegetation". Latitude and longitude are always provided. Default = "Elevation"
#' @return a 2 elements named list : (1) snitch and (2) output. snitch is TRUE if function has provided the expected result. output is a named list which contains 3 elements :
#' (1) value : a list of dataframes where each dataframe contains the sid, the mtime, the sensor data, x position (longitude), y position (latitude) and the explanatory variables
#' (2) condition : a character specifying if the functions has encountered success, warning, error
#' (3) message : the message relative to the condition
#' @examples
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T15:00:00Z",
#'   sensor = "tsa")
#'
makeDataset <- function(
  user_token = Sys.getenv("AGROMET_API_V1_KEY"),
  stations,
  json = NULL,
  dfrom = NULL,
  dto = NULL,
  sensor = "tsa",
  staticExpl = "elevation",
  dynExpl = NULL
){

    output = list(value = NULL, condition = list(type = NULL, message = NULL))
    snitch = FALSE

    doMakeDataset = function(){
      message("Making dataset...")

      #browser()

      if (is.null(json)) {
        # make an API call to retrieve the dynamic data
        message("Calling Agromet API...")

        dataset = typeData(
          getData(user_token = user_token,
            dfrom = dfrom,
            dto = dto,
            sensors = paste0(sensor, collapse = ","),
            sid = paste0(stations, collapse = "," ))
        )

      } else{
        # read the json FILE
        message("Reading JSON file...")
        dataset = jsonlite::fromJSON(json)
        dataset.data = dataset$results
        dataset.meta = dataset$references$stations
        dataset = list(metadata = dataset.meta, data = dataset.data)
        dataset = typeData(dataset, "cleandata")

        dataset = dataset %>%
          dplyr::filter(network_name == "pameseb") %>%
          dplyr::filter(type_name != "Sencrop") %>%
          dplyr::filter(state == "Ok") %>%
          tidyr::drop_na()
      }

      # Keep only the relevant columns
      dataset = dataset %>%
        dplyr::select("sid", "mtime", sensor) %>%
        dplyr::mutate(task.id = gsub("[^[:digit:]]", "", dataset$mtime))

      # join with static explanatory vars
      dataset = dataset %>%
        dplyr::left_join(
          stations.df %>%
            dplyr::select(c("sid", "X", "Y", staticExpl)),
          by = "sid")

      # rename X and Y to x and y for mlr (gstat learner compatibility
      dataset = dataset %>%
        dplyr::rename("y" = "Y") %>%
        dplyr::rename("x" = "X")

      # group by task.id and make lists of dataframes
      dataset = split(
        x = dataset,
        f = as.factor(dataset$task.id))


      # remove the task.id column from each dataset
      dataset = lapply(dataset,
        function(x){
          x = x %>%
            dplyr::select(c(-task.id))
        })

      return(dataset)
    }

    tryCatch(

    expr = {

      # check if usertoken provided
      stopifnot(!is.null(user_token))
      # check if sensor provided is OK
      stopifnot(sensor %in% c("tsa", "hra", "hct", "vvt", "ens", "plu" , "sunrise", "sunset"))
      # check if staticExpl provided is ok
      stopifnot(all(staticExpl %in% colnames(stations.df[3:9])))
      # check if queried stations exist.::todo:: better dynamic check of exisitng stations
      # stopifnot(isTRUE(all(strsplit(stations, ",")[[1]]  %in% (as.character(stations.df$sid)))))

      # in case everything went fine do makeDataset
      output$value = doMakeDataset()
      output$condition$type = "success"
      output$condition$message = "Dataset created"
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::makeDataset raised a warning -> ",
        w)
      snitch <<- TRUE
      output$value <<- doMakeTask()
      output$condition$type <<- "warning"
      output$condition$message <<- warning

    },
    error = function(e){
      error = paste0(
        "AgrometeoR::makeDataset raised an error -> ",
        e)
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "makeDataset has encountered : ",
        output$condition$type,
        ". \n",
        "All done with makeDataset. "
      )
      message(finalMessage)
      return(list(snitch = snitch, output = output))
    }
  )
}

