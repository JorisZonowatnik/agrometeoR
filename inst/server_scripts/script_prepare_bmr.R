#!/usr/bin/env Rscript
# to execute it from terminal. simply type use Rscript ./script_execute_bmr.R ./data-created/tasksPamesebForBmrs.rds hourly_tsa_Pameseb

# get data from FTP and save it in the current WD - should work if the port is open
# threadr::download_ftp_file(
#   file_remote = "ftp://178.32.44.217/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json",
#   file_output = "./data-raw/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json",
#   credentials = paste0("agromet:",
#     Sys.getenv("FTP_PASSWORD")),
#   curl = FALSE, verbose = FALSE, progress = "none")

# to execute it from terminal. simply type use Rscript ./script_execute_bmr.R ./data-created/tasksPamesebForBmrs.rds hourly_tsa_Pameseb
# https://github.com/IARCbioinfo/R-tricks#use-rscript-to-run-r-from-bash
# http://www.milanor.net/blog/bashr-howto-pass-parameters-from-bash-script-to-r/

# parsing the args passed to bash CLI
args = commandArgs(trailingOnly=TRUE)
message(paste0("The working directory is ", getwd()))
message("The demanded task file is ", args[1])
message("Your output file will be prefixed by : ",args[2])

tasks.file = args[1]
output.name = args[2]


# load the agrometeor library
library(agrometeoR)
library(purrr)
library(dplyr)

#####
## preparing the tasks for both IRM + Pameseb Tsa benchmark hourly

# check if dataPamesebIrmForBmrs R object already exists else create it
files = list.files("./data-created")
if ("dataPamesebIrmForBmrs.rds" %in% files) {
  dataPamesebIrmForBmrs = readRDS("./data-created/dataPamesebIrmForBmrs.rds")
}else{
  dataPamesebIrmForBmrs = makeDataset(json = "./data-raw/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json", sensor = "tsa" )
  dataPamesebIrmForBmrs = dataPamesebIrmForBmrs$output$value
  saveRDS(object = dataPamesebIrmForBmrs, file = "./data-created/dataPamesebIrmForBmrs.rds")
}
# check if tasks already exists else create it
if ("dataPamesebIrmForBmrs.rds" %in% files) {
  readRDS("./data-created/dataPamesebIrmForBmrs.rds")
}else{
  dataPamesebIrmForBmrs = purrr::map(dataForBmrs, makeTask, target = "tsa")
  saveRDS(object = dataPamesebIrmForBmrs, file = "./data-created/dataPamesebIrmForBmrs.rds")
}

#####
## preparing the tasks for Pameseb only TSA benchmark hourly

# check if dataPamesebForBmrs R object already exists else create it
files = list.files("./data-created")
if ("dataPamesebForBmrs.rds" %in% files) {
  dataPamesebForBmrs = readRDS("./data-created/dataPamesebForBmrs.rds")
}else{
  # created a filtered version of dataForBmrs to only keep Pameseb data
  dataPamesebForBmrs = dataForBmrs %>% purrr::map(., ~dplyr::filter(.,sid < 1000))
  saveRDS(object = dataPamesebForBmrs, file = "./data-created/dataPamesebForBmrs.rds")
}
# check if tasks already exists
if ("tasksPamesebForBmrs.rds" %in% files) {
  readRDS("./data-created/tasksPamesebForBmrs.rds")
}else{
  tasksPamesebForBmrs = purrr::map(dataPamesebForBmrs, makeTask, target = "tsa")
  saveRDS(object = tasksPamesebForBmrs, file = "./data-created/tasksPamesebForBmrs.rds")
}

#####
## create the daily datasets and tasks for Pameseb and Pameseb + IRm
split_tibble <- function(tibble, column = 'col') {
  tibble %>% split(., .[,column]) %>% lapply(., function(x) x[,setdiff(names(x),column)])
}

summarize_by_datetime = function(l){
  dataset = l %>% purrr::map_df(.,c, .id = "datetime")
  a = dataset %>%
    dplyr::mutate_at("datetime", function(x){
      x = substr(x, start = 1, stop = 8)
    })

  b = a %>% group_by(datetime, sid) %>%
    summarise(
      tsa_min = min(tsa, na.rm = TRUE),
      tsa_max = max(tsa, na.rm = TRUE),
      tsa_mean = mean(tsa, na.rm = TRUE))

  b = b %>%
    dplyr::rename("mtime" = "datetime")

  b = b %>%
    dplyr::left_join(dplyr::select(stations.df, one_of(c("x", "y", "elevation", "sid"))), by = "sid")

  c = b %>% split_tibble("mtime")

  d = lapply(seq_along(c), function(x){
    mtime = names(c)[x]
    mtime = rep.int(mtime, nrow(c[[x]]))
    df = c[[x]] %>%
      dplyr::mutate(mtime = mtime)
    return(df)
  })

  names(d)= names(c)
  d = d %>% purrr::map(as.data.frame)
  return(d)
}

dataPamesebDailyForBmrs = dataPamesebForBmrs %>% summarize_by_datetime()
dataPamesebIrmDailyForBmrs = dataPamesebIrmForBmrs %>% summarize_by_datetime()

saveRDS(object = dataPamesebDailyForBmrs, file = "./data-created/dataPamesebDailyForBmrs.rds")
saveRDS(object = dataPamesebIrmDailyForBmrs, file = "./data-created/dataPamesebIrmDailyForBmrs.rds")

tasksdataPamesebDailyForBmrs = purrr::map(dataPamesebDailyForBmrs, makeTask, target = "tsa_max")
tasksdataPamesebIrmDailyForBmrs = purrr::map(dataPamesebIrmDailyForBmrs, makeTask, target = "tsa_max")

saveRDS(object = tasksdataPamesebDailyForBmrs, file = "./data-created/tasksdataPamesebDailyForBmrs.rds")
saveRDS(object = tasksdataPamesebIrmDailyForBmrs, file = "./data-created/tasksdataPamesebIrmDailyForBmrs.rds")

# checkNrowEqual = function(x){
#   nrow(x) == nrow(dataForBmrs$`20160101000000`)
# }
#
# checkNAPresence = function(x){
#   nrow(na.omit(x)) == nrow(x)
# }
#
# isNrowEqual = lapply(dataForBmrs, checkNrowEqual)
# cond = sapply(isNrowEqual, function(x) !isTRUE(x))
# notEqual = isNrowEqual[cond]
# length(notEqual)
#
#
# isNaPresent = lapply(dataForBmrs, checkNAPresence)
# cond = sapply(isNaPresent, function(x) isTRUE(x))
# containsNA = isNaPresent[cond]
# length(containsNA)


