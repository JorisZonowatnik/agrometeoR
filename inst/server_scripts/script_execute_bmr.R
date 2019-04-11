#!/usr/bin/env Rscript
# to execute it from terminal. simply type $ ./script_execute_bmr.R

# load the libraries
message("loading the required libs")
library(parallelMap)
library(mlr)
library(agrometeoR)

# load the data for the bmrs
message("loading the dataset")
readRDS("./tasksForBmr.rds")

# extract the required part of the tasksForBmr object
tasksOutputs = tasksForBmr %>% modify_depth(1, ~.$"output"$"value"$"task")

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
saveRDS(object = bmrsResult, file = "./data-created/bmrsResult.rds")

# purge the memory
rm(list = ls())
