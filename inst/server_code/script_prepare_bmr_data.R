#!/usr/bin/env Rscript

# https://github.com/IARCbioinfo/R-tricks#use-rscript-to-run-r-from-bash

# get data from FTP and save it in the current WD - should work if the port is open
# threadr::download_ftp_file(
#   file_remote = "ftp://178.32.44.217/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json",
#   file_output = "spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json",
#   credentials = paste0("agromet:",
#     Sys.getenv("FTP_PASSWORD")),
#   curl = FALSE, verbose = FALSE, progress = "none")

library(agrometeoR)

dataset = makeDataset(json = "./data-raw/spCleandataSensorstsaForallFm2016-01-01To2017-12-31.json", sensor = "tsa" )
dataset = dataset$output$value
save.image(file = "dataForBmr.RData")

checkNrowEqual = function(x){
  nrow(x) == nrow(dataset$`20160101000000`)
}

checkNAPresence = function(x){
  nrow(na.omit(x)) == nrow(x)
}

isNrowEqual = lapply(dataset, checkNrowEqual)
cond = sapply(isNrowEqual, function(x) !isTRUE(x))
notEqual = isNrowEqual[cond]
length(notEqual)


isNaPresent = lapply(dataset, checkNAPresence)
cond = sapply(isNaPresent, function(x) isTRUE(x))
containsNA = isNaPresent[cond]
length(containsNA)

load("~/Rprojetcs/bmr_april/dataForBmr.RData")

# extarcting a small part from teting purposes
dataset = dataset

# Creating the tasks
tasks = purrr::map(dataset, makeTask, target = "tsa")

# removing the useless part of each task
tasks_20_pameseb_2bmr =  tasks_20_pameseb %>% purrr::modify_depth(1, ~.$"output"$"value"$"task")

save.image(file = "tasksForBmr.RData")