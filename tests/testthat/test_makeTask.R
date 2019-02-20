
library(testthat)
context("Testing makeTask")

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
      dataset = test_bad_dataset,
      drop = NULL,
      target = "foo"
    )),
  warning = list(
    list(
      id = "dataset_with_NA",
      dataset = test_dataset_with_NA,
      drop = NULL,
      target = "tsa"
    ))
)

test_that("Output has the good structure, whatever error or not", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeTask, args = groups[[group]][[case]][-1])
      expect_is(object, class = "list")
      expect_length(object, 2)
      expect_equal(names(object), c("bool", "output"))
    }
  }
})

test_that("Error are thrown when a bad parameter is passed", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "bad") {
      for (case in 1:length(group)) {
        browser()
        object = do.call(what = makeTask, args = groups[[group]][[case]][-1])
        expect_error(object)
      }
    }
  }
})

test_that("Warning is thrown when dataset contains NA values", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "warning") {
      for (case in 1:length(group)) {
        browser()
        object = do.call(what = makeTask, args = groups[[group]][[case]][-1])
        expect_warning(object)
      }
    }
  }
})

test_that("When bool isTRUE (no error), output has class RegrTask and when bool isFALSE, output has class NULL", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeTask, args = groups[[group]][[case]][-1])
      if (isTRUE(object$bool)) {
        expect_is(object$output$value, "RegrTask")
      } else {
        expect_null(object$output$value)
      }
    }
  }
})


