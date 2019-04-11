#!/usr/bin/env Rscript
# to execute it from terminal. simply type $ ./script_execute_bmr.R

# load the libraries
message("loading the required libs")
suppressMessages(library(parallelMap))
suppressMessages(library(mlr))
suppressMessages(library(agrometeoR))
suppressMessages(library(dplyr))


# load the data for the bmrs
message("loading the tasks")
tasksForBmrs = readRDS("./data-created/tasksForBmrs.rds")

# extract the required part of the tasksForBmr object
tasksOutputs = tasksForBmrs %>% purrr::modify_depth(1, ~.$"output"$"value"$"task")

# perform the benchmarks
message("starting to conduct the benchmarks")
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
  prefix = "",
  output_dir = paste0("./bmrs-output/", as.character(Sys.Date())),
  removeTemp = FALSE
)

# save the bmrs result
message("Saving the result...")
saveRDS(object = bmrsResult, file = paste0(
  "./data-created/",
  as.character(Sys.Date()),
  "_bmrsResult.rds"))

# purge the memory
rm(list = ls())