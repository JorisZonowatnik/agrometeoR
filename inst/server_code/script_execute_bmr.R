#!/usr/bin/env Rscript

# to execute it from terminal. simply type $ ./makeBenchmark.R


# load the data for the bmrs
message("loading the data")
load("~/Rprojetcs/bmr_april/tasksForBmr.RData")

# load the libraries
message("loading the required libs")
library(parallelMap)
library(mlr)
library(agrometeoR)

# https://stackoverflow.com/questions/15668893/r-multicore-mcfork-unable-to-fork-cannot-allocate-memory
message("starting to conduct the benchamrks")
bmr = makeBatchOfBenchExp(
  tasks = tasks,
  learners = agrometeorLearners,
  measures = list(rmse, mae, mse),
  keep.pred = TRUE,
  models = FALSE,
  grouping = 100,
  level = "mlr.benchmark",
  resamplings = "LOO",
  cpus = 8,
  prefix = "test_april_",
  output_dir = "./bmrs/april_big/",
  removeTemp = FALSE
)


save.image(file = paste0( "./bmrs/april_big/", "resultsOfBmr.RData"))
# rm(bmr)
