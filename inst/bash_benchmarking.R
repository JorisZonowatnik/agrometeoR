#!/usr/bin/env Rscript

# command to use in bash :
# bmr=$( Rscript --vanilla <PATH_TO_bash_benchmarking.R> <PATH_TO_JSON> <SENSOR_NAME> <BMR_FILE_NAME>)

#parsing the CLI args
args = commandArgs(trailingOnly = TRUE)

# install.packages("../agrometeoR", repos = NULL, type="source")
library(agrometeoR)

dataset = makeDataset(json = args[1], sensor = args[2] )

tasks = makeTasks(dataset = dataset$output$value, target = "tsa")

bmr = makeBenchmark(
  models = FALSE,
  keep.pred = FALSE,
  tasks = tasks$output$value,
  learners = list(
    am.learners$lrn.lm.alt,
    am.learners$lrn.gstat.idw,
    am.learners$lrn.gstat.krige,
    am.learners$lrn.gstat.krige.alt,
    am.learners$lrn.gstat.knn.alt.tuning
  ))

save.image(file = paste0(args[3], ".RData"))