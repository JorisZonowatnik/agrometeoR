
library(testthat)
context("Testing exportSpatialization")

groups = list(
  good = list(
    list(
      id = "good",
      spatialized = test_spatialized,
      path = paste0(getwd(), "/test/testthat/exports/"),
      filename = NULL,
      format = "csv",
      write = FALSE
    )),
  bad = list(
    list(
      id = "bad_spatialized",
      spatialized = test_bad_spatialized,
      path = paste0(getwd(), "/test/testthat/exports/"),
      filename = NULL,
      format = "csv",
      write = FALSE
    ),
    list(
      id = "bad_format",
      spatialized = test_spatialized,
      path = paste0(getwd(), "/test/testthat/exports/"),
      filename = NULL,
      format = "xls",
      write = FALSE
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


