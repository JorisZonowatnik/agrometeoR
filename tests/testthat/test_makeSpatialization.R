
library(testthat)
context("Testing makeSpatialization")

groups = list(
  good = list(
    list(
      id = "good",
      model = test_models,
      pred.grid = test_grid
    )),
  bad = list(
    list(
      id = "bad_grid",
      model = test_models,
      pred.grid = test_bad_grid
    ),
    list(
      id = "bad_model",
      model = "bad_model",
      pred.grid = test_grid
    )
  ))

test_that("Output has the good structure, whatever error or not", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeSpatialization, args = groups[[group]][[case]][-1])
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
        object = do.call(what = makeSpatialization, args = groups[[group]][[case]][-1])
        expect_error(object)
      }
    }
  }
})

test_that("When bool isTRUE (no error), output has class dataframe and when bool isFALSE, output has class NULL", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeSpatialization, args = groups[[group]][[case]][-1])
      if (isTRUE(object$bool)) {
        expect_is(object$output$value, "data.frame")
      } else {
        expect_null(object$output$value)
      }
    }
  }
})

test_that("When bool isTRUE (no error), number of rows of output dataframe is equal to number of rows of input dataframe", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeSpatialization, args = groups[[group]][[case]][-1])
      if (isTRUE(object$bool)) {
        expect_identical(nrow(object$output$value), nrow(test_grid))
      }
    }
  }
})

test_that("When bool isTRUE (no error), px references of output dataframe are equal to px references of input dataframe", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeSpatialization, args = groups[[group]][[case]][-1])
      if (isTRUE(object$bool)) {
        expect_identical(unique(object$output$value$px), unique(test_grid$px))
      }
    }
  }
})


