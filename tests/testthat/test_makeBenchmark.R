
library(testthat)
context("Testing makeBenchmark")

groups = list(
  good = list(
    list(
      id = "good",
      parallel = TRUE,
      cpus = 2,
      tasks = test_tasks,
      learners = learners[c(1, 2, 6, 8)],
      resamplings = "LOO")
  ),
  bad = list(
    list(
      id = "bad",
      parallel = TRUE,
      cpus = NULL,
      tasks = test_tasks,
      learners = "learners",
      resamplings = "LOO")))

test_that("Output has the good structure, whatever error or not", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeBenchmark, args = groups[[group]][[case]][-1])
      expect_is(object, class = "list")
      expect_length(object, 2)
      expect_equal(names(object), c("bool", "output"))
    }
  }
})

test_that("Error are thrown when a bad parameter is passed", {
  for (group in 1:length(groups)) {
    if (names(groups[1]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = makeBenchmark, args = groups[[group]][[case]][-1])
        expect_error(object)
      }
    }
  }
})

test_that("When bool isTRUE (no error), output has class list and when bool isFALSE, output has class NULL", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      args = groups[[group]][[case]][-1]
      object = do.call(what = makeBenchmark, args = args)
      browser()
      if (isTRUE(object$bool)) {
        expect_is(object$output$value, "list")
      } else {
        expect_null(object$output$value)
      }
    }
  }
})


