
library(testthat)
context("Testing makeDataset")

groups = list(
  good = list(
    list(
      id = "good",
      stations = test_stations,
      user_token = test_user_token,
      dfrom = test_dfrom,
      dto = test_dto,
      sensor = test_sensor,
      staticExpl = test_staticExpl,
      json = NULL,
      dynExpl = NULL)
    ),
  bad = list(
     list(
      id = "bad_user_token",
      stations = test_stations,
      user_token = test_bad_user_token,
      dfrom = test_dfrom,
      dto = test_dto,
      sensor = test_sensor,
      staticExpl = test_staticExpl,
      json = NULL,
      dynExpl = NULL),
    list(
      id = "bad_stations",
      stations = test_bad_stations,
      user_token = test_user_token,
      dfrom = test_dfrom,
      dto = test_dto,
      sensor = test_sensor,
      staticExpl = test_staticExpl,
      json = NULL,
      dynExpl = NULL),
    list(
      id = "bad_dfrom",
      stations = test_stations,
      user_token = test_user_token,
      dfrom = test_bad_dfrom,
      dto = test_dto,
      sensor = test_sensor,
      staticExpl = test_staticExpl,
      json = NULL,
      dynExpl = NULL),
    list(
      id = "bad_dto",
      stations = test_stations,
      user_token = test_user_token,
      dfrom = test_dfrom,
      dto = test_bad_dto,
      sensor = test_sensor,
      staticExpl = test_staticExpl,
      json = NULL,
      dynExpl = NULL),
    list(
      id = "bad_sensor",
      stations = test_stations,
      user_token = test_user_token,
      dfrom = test_dfrom,
      dto = test_dto,
      sensor = test_bad_sensor,
      staticExpl = test_staticExpl,
      json = NULL,
      dynExpl = NULL),
    list(
      id = "bad_staticExpl",
      stations = test_stations,
      user_token = test_user_token,
      dfrom = test_dfrom,
      dto = test_dto,
      sensor = test_sensor,
      staticExpl = test_bad_staticExpl,
      json = NULL,
      dynExpl = NULL)))

test_that("Output has the good structure, whatever error or not", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeDataset, args = groups[[group]][[case]][-1])
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
        object = do.call(what = makeDataset, args = groups[[group]][[case]][-1])
        expect_error(object)
      }
    }
  }
})

test_that("When bool isTRUE (no error), output has class dataframe and when bool isFALSE, output has class NULL", {
  for (group in 1:length(groups)) {
      for (case in 1:length(group)) {
        object = do.call(what = makeDataset, args = groups[[group]][[case]][-1])
        if (isTRUE(object$bool)) {
          expect_is(object$output$value, "data.frame")
        } else {
          expect_null(object$output$value)
        }
      }
    }
})


