#' @export
#' @title make a dataframe of stations records for each requested hour
#' @author Thomas Goossens
#' @param token a character specifying your agrometAPI token
#' @param stations a character vector specifying the sid's of the stations to use
#' @param json a character specifying the path of a json file constructed from a batch db export.
#' If NULL the API will be called.
#' @param dfrom a datetime string specifying the dateTime from which you want data
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param dto a datetime string specifying the dateTime to which you want data
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param sensors a character vector specifying the name of the sensors for which you data.
#' One of tsa, hct, hra, ens, plu, vvt.
#' @param dynExpl a character vector specifying the dynamic explanatory variables you want to add to the task.
#' Any combinations of inca, ens
#' @param staticExpl a character vector specifying the desired static explanatory variables
#' Any combinations of "elevation", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest", "Herbaceous_vegetation". Latitude and longitude are always provided. Default = "Elevation"
#' @return a 2 elements named list : (1) snitch and (2) output. snitch is TRUE if function has provided the expected result. output is a named list which contains 3 elements :
#' (1) value : a list of dataframes where each dataframe contains the sid, the mtime, the sensor data, x position (longitude), y position (latitude) and the explanatory variables
#' (2) condition : a character specifying if the functions has encountered success, warning, error
#' (3) message : the message relative to the condition
#' @examples
#'\dontrun{
#' # get the dataset
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T15:00:00Z",
#'   sensor = "tsa")
#'
#' # extract the data
#' myDataset = myDataset$output$value
#'
#' # each dataframe is stored in a list
#' class(myDataset)
#'
#' # show the head of the first dataframe
#' head(myDataset[[1]])
#'}
#'
makeDataset <- function(
  user_token = Sys.getenv("AGROMET_API_V1_KEY"),
  api_request = "https://app.pameseb.be/agromet/api/v2/sp/cleandata",
  sensors = "tsa",
  stations = "all",
  dfrom = NULL,
  dto = NULL,
  json = NULL,
  staticExpl = "elevation",
  dynExpl = NULL
){

    output = list(value = NULL, condition = list(type = NULL, message = NULL))
    snitch = FALSE

    doMakeDataset = function(){
      message("Making dataset...")

      if (is.null(json)) {
        # make an API call to retrieve the dynamic data
        message("Calling Agromet API...")

        # Clean the eventual spaces in the sensors string
        if (!is.null(sensors)) {
          sensors = gsub(" ","",sensors)
        }
        # Building the API call URL
        api_call = paste(
          api_request,
          sensors, stations, dfrom, dto, sep = "/")

        message(paste("Your API URL call is : ", api_call, " \n"))

        # Add user token into the HTTP authentication header and call API (https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
        api_resp = httr::GET(
          api_call,
          httr::add_headers("Authorization" = paste("Token", user_token, sep = " ")))

        # Check the status of the API response
        if (api_resp$status_code != 200) {
          stop(paste0("The API responded with an error ", api_resp$status_code, ". Please check your token and the validity + order of the parameters you provided. API documentation available at https://app.pameseb.be/fr/pages/api_call_test/" ))
        }else {
          message(paste0("The API responded with a status code ", api_resp$status_code, ". \n Data has been downloaded. \n"))
        }

        # Getting the JSON data from the API response
        api_json = httr::content(api_resp, as = "text", encoding = "UTF-8")

        # Transform the JSON response to R-friendly list format
        dataset = jsonlite::fromJSON(api_json)

      }else {# ! is.null json
        # read the json FILE
        message("Reading JSON file...")
        dataset = jsonlite::fromJSON(json)
      }


      # keeping only what we need
      dataset = list(references.stations = dataset$references$stations, results = dataset$results)

      # Keeping what we need in results
      good_columns = c("mtime", "sid", sensors) # paste0(sensors, "state")
      dataset$results = dataset$results %>%
        dplyr::select(dplyr::one_of(good_columns)) %>%
        dplyr::mutate_at("sid", as.character)

      # keeping what we need in stations medatadata
      good_columns = c("sid", "poste", "network_name", "type_name", "state")
      dataset$references.stations = dataset$references.stations %>%
        dplyr::select(dplyr::one_of(c(good_columns))) %>%
        # filtering to only keep the good stations
        dplyr::filter(network_name == "pameseb" | network_name == "irm") %>%
        dplyr::filter(type_name != "Sencrop") %>%
        dplyr::filter(state == "Ok") %>%
        dplyr::mutate_at("sid", as.character)

      # joining the results and the stations metadata
      dataset = dataset$results %>%
        dplyr::filter(sid %in% dataset$references.stations$sid) %>%
        dplyr::mutate_at("sid", as.numeric) #hack to join with static explanatory vars

      # join with static explanatory vars
      dataset = dataset %>%
        dplyr::left_join(
          stations.df %>%
            dplyr::select(c("sid", "x", "y", staticExpl)),
          by = "sid")

      # Transform mtime column to posix format for easier time handling
      dataset = dataset %>%
        dplyr::mutate_at(.vars = vars(mtime), as.POSIXct, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")

      # Transform sid, sensors cols, altitude, longitude, latitude columns from character to numeric
      dataset = suppressWarnings(
        dataset %>%
          dplyr::mutate_at(dplyr::vars(dplyr::one_of(c(sensors, "altitude", "x", "y", "sid"))), dplyr::funs(as.numeric)))

      # Create a task.id column for later task splittings
      dataset = dataset %>%
        dplyr::mutate(task.id = gsub("[^[:digit:]]", "", dataset$mtime))

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
      stopifnot(sensors %in% c("tsa", "hra", "hct", "vvt", "ens", "plu" , "sunrise", "sunset"))
      # check if staticExpl provided is ok :todo::
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
      output$value <<- doMakeDataset()
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

