
library(testthat)
context("Testing makeTasks")

groups = list(
  good = list(
    list(
      id = "good",
      dataset = test_dataset,
      drop = NULL,
      target = "tsa"
      )),
  bad = list(
    list(
      id = "bad_target",
      dataset = test_dataset,
      drop = NULL,
      target = "foo"
    ),
    list(
      id = "bad_dataset",
      dataset = test_bad_dataset,
      drop = NULL,
      target = "tsa"
    )
  ))

test_that("Output has the good structure, whatever error or not", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeTasks, args = groups[[group]][[case]][-1])
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
        object = do.call(what = makeTasks, args = groups[[group]][[case]][-1])
        expect_error(object)
      }
    }
  }
})

test_that("When bool isTRUE (no error), output has class RegrTask and when bool isFALSE, output has class NULL", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeTasks, args = groups[[group]][[case]][-1])
      if (isTRUE(object$bool)) {
        expect_is(object$output$value, "RegrTask")
      } else {
        expect_null(object$output$value)
      }
    }
  }
})


