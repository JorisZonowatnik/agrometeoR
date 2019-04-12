#!/usr/bin/env Rscript

# to execute it from terminal. simply type $ Rscript ./script_execute_bmr.R <tasks_file_path> <output_name>
# ex : tgoossens@agromettest:~/Rprojetcs/benchmarks_thomas$ ./script_execute_bmr.R ./dataPamesebForBmrs.rds hourly_tsa_Pameseb

# https://github.com/IARCbioinfo/R-tricks#use-rscript-to-run-r-from-bash
# http://www.milanor.net/blog/bashr-howto-pass-parameters-from-bash-script-to-r/

# parsing the args passed to bash CLI
args = commandArgs(trailingOnly = TRUE)
task.file = args[1]
output.name = args[2]

# test if the task file exist else return an error
stopifnot(file.exists(tasks.file))

# load the libraries
message("loading the required libs")
suppressMessages(library(parallelMap))
suppressMessages(library(mlr))
suppressMessages(library(agrometeoR))
suppressMessages(library(dplyr))

# load the data for the bmrs
message(paste0("Reading and loading the tasks file", tasks.file))
tasksForBmrs = readRDS(paste0("./data-created/", task.file))

# extract the required part of the tasksForBmr object
tasksOutputs = tasksForBmrs %>% purrr::modify_depth(1, ~.$"output"$"value"$"task")

# perform the benchmarks
message("Starting to conduct the benchmarks")
bmrsResult = makeBatchOfBenchExp(
  tasks = tasksOutputs,
  learners = agrometeorLearners,
  measures = list(rmse, mae, mse),
  keep.pred = TRUE,
  models = FALSE,
  grouping = 100,
  level = "mlr.benchmark",
  resamplings = "LOO",
  cpus = 8,
  prefix = output.name,
  output_dir = paste0("./bmrs_tempFiles/", as.character(Sys.Date())),
  removeTemp = FALSE
)

# save the bmrs result
message("Saving the result...")
saveRDS(object = bmrsResult, file = paste0(
  "./data-created/",
  as.character(Sys.Date()),
  prefix,
  "_bmrsResult.rds"))

# purge the memory
rm(list = ls())