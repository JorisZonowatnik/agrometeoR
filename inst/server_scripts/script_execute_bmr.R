#!/usr/bin/env Rscript

# to execute it from terminal. simply type use Rscript ./script_execute_bmr.R ./data-created/tasksdataPamesebIrmDailyForBmrs.rds daily_tsa_PamesebIrm
# https://github.com/IARCbioinfo/R-tricks#use-rscript-to-run-r-from-bash
# http://www.milanor.net/blog/bashr-howto-pass-parameters-from-bash-script-to-r/
#
# ```{bash}
# ssh agromet
# tmux ls #list
#
# tmux new-session -s bmr
#
# cd Rprojects/currentProject/
#
#   tmux attach
#
# chmod +x script_execute_bmr.R # must load it via system.file
#
# ./makeBenchmark
#
# # type ctrl+b et puis d => hide the plex
#
# tmux attach
#
# # type exit to kill a plex
# ```

# parsing the args passed to bash CLI
args = commandArgs(trailingOnly=TRUE)
message(paste0("The working directory is ", getwd()))
message("The demanded task file is ", args[1])
message("Your output file will be prefixed by : ",args[2])

# args = c("./data-created/tasksdataPamesebIrmDailyForBmrs.rds", "daily_tsa_PamesebIrm")

tasks.file = args[1]
output.name = args[2]

# test if the task file exist else return an error
stopifnot(file.exists(paste0(tasks.file)))

# load the libraries
message("loading the required libs")
suppressMessages(library(parallelMap))
suppressMessages(library(mlr))
suppressMessages(library(agrometeoR))
suppressMessages(library(dplyr))

# load the data for the bmrs
message(paste0("Reading and loading the tasks file", tasks.file))
tasksForBmrs = readRDS(tasks.file)

# extract the required part of the tasksForBmr object
tasksOutputs = tasksForBmrs %>% purrr::modify_depth(1, ~.$"output"$"value"$"task")

# perform the benchmarks
message("Starting to conduct the benchmarks")
bmrsResult = makeBmrsBatch(
  tasks = tasksOutputs[1:25],
  learners = agrometeorLearners,
  measures = list(rmse, mae, mse),
  keep.pred = TRUE,
  models = FALSE,
  groupSize = 100,
  level = "mlr.benchmark",
  resamplings = "LOO",
  cpus = 8,
  prefix = output.name,
  temp_dir = paste0("./bmrs_tempFiles/", as.character(Sys.Date())),
  removeTemp = FALSE
)

# save the bmrs result
message("Saving the result...")
saveRDS(object = bmrsResult, file = paste0(
  "./data-created/",
  output.name,
  as.character(Sys.Date()),
  "_bmrsResult.rds"))

# purge the memory
# rm(list = ls())
