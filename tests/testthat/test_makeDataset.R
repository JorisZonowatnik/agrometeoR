# https://stackoverflow.com/questions/37870941/r-trycatch-with-testthat-expectation
# https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html#better-error-handling-with-the-trycatchlog-package
# https://bookdown.org/rdpeng/RProgDA/error-handling-and-generation.html
# https://rsangole.netlify.com/post/try-catch/

#####
## unit tests for makeDataset
## to run these tests, use : devtools::test(filter= "makeDataset")
## in case you want to debug on of these tests, place browser() where yo uwant the code execution to stop

#####
## library and context.
library(testthat)
context("Testing makeDataset")

#####
## definition of the various function inputs that are tested.
## the objects used in these function inputs definition comes precompiled. See fodler data-raw for their source file

groups = list(
  good = list(
    all_good = list(
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
    bad_user_token = list(
      stations = test_stations,
      user_token = test_bad_user_token,
      dfrom = test_dfrom,
      dto = test_dto,
      sensor = test_sensor,
      staticExpl = test_staticExpl,
      json = NULL,
      dynExpl = NULL),
  bad_stations = list(
    stations = test_bad_stations,
    user_token = test_user_token,
    dfrom = test_dfrom,
    dto = test_dto,
    sensor = test_sensor,
    staticExpl = test_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_dfrom = list(
    stations = test_stations,
    user_token = test_user_token,
    dfrom = test_bad_dfrom,
    dto = test_dto,
    sensor = test_sensor,
    staticExpl = test_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_dto = list(
    stations = test_stations,
    user_token = test_user_token,
    dfrom = test_dfrom,
    dto = test_bad_dto,
    sensor = test_sensor,
    staticExpl = test_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_sensor = list(
    stations = test_stations,
    user_token = test_user_token,
    dfrom = test_dfrom,
    dto = test_dto,
    sensor = test_bad_sensor,
    staticExpl = test_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_staticExpl = list(
    stations = test_stations,
    user_token = test_user_token,
    dfrom = test_dfrom,
    dto = test_dto,
    sensor = test_sensor,
    staticExpl = test_bad_staticExpl,
    json = NULL,
    dynExpl = NULL)))


#####
## definition of the unit tests

# test1
test_outputStructure = function(){test_that("Output has the good structure whatever the inputs", {
  for (group in 1:length(groups)) {
      for (case in 1:length(group)) {
        object = do.call(what = makeDataset, args = groups[[group]][[case]])
        expect_is(object, class = "list")
        expect_length(object, 2)
        expect_named(object, c("snitch", "output"))
        expect_named(object$output, c("value", "condition"))
      }
  }
})}


# test2
test_badInput = function(){test_that("Expected behaviour in case of bad parameter", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = makeDataset, args = groups[[group]][[case]])
        expect_false(object$snitch)
        expect_equal(object$output$condition$type, "error")
        expect_null(object$output$value)
      }
    }
  }
})}

# test3
test_goodInput = function(){test_that("Expected behaviour in case of good parameters", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "good") {
      for (case in 1:length(group)) {
        object = do.call(what = makeDataset, args = groups[[group]][[case]])
        expect_true(object$snitch)
        expect_is(object$output$value[[1]], class = "data.frame")
        expect_gte(nrow(object$output$value[[1]]), 1)
      }
    }
  }
})}

#####
## execution of the tests. If you want to skip a test, simply comment it :)

test_outputStructure()
test_badInput()
test_goodInput()


