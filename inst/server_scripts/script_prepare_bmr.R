#!/usr/bin/env Rscript
# https://github.com/IARCbioinfo/R-tricks#use-rscript-to-run-r-from-bash

# get data from FTP and save it in the current WD - should work if the port is open
# threadr::download_ftp_file(
#   file_remote = "ftp://178.32.44.217/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json",
#   file_output = "./data-raw/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json",
#   credentials = paste0("agromet:",
#     Sys.getenv("FTP_PASSWORD")),
#   curl = FALSE, verbose = FALSE, progress = "none")


# load the agrometeor library
library(agrometeoR)

# check if dataForBmrs R object already exists
files = list.files("./data-created")
if ("dataForBmrs.rds" %in% files) {
  readRDS("./data-created/dataForBmrs.rds")
}else{
  dataForBmrs = makedataForBmrs(json = "./data-raw/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json", sensor = "tsa" )
  dataForBmrs = dataForBmrs$output$value
  saveRDS(object = dataForBmrs, file = "./data-created/dataForBmrs.rds")
}

# check if tasks already exists
if ("tasksForBmrs.rds" %in% files) {
  readRDS("./data-created/tasksForBmrs.rds")
}else{
  tasksForBmrs = purrr::map(dataForBmrs, makeTask, target = "tsa")
  saveRDS(object = dataForBmrs, file = "./data-created/dataForBmrs.rds")
}

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


