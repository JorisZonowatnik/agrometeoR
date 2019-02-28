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
      stations = ex_stations,
      user_token = ex_user_token,
      dfrom = ex_dfrom,
      dto = ex_dto,
      sensor = ex_sensor,
      staticExpl = ex_staticExpl,
      json = NULL,
      dynExpl = NULL)
    ),
  bad = list(
    bad_user_token = list(
      stations = ex_stations,
      user_token = ex_bad_user_token,
      dfrom = ex_dfrom,
      dto = ex_dto,
      sensor = ex_sensor,
      staticExpl = ex_staticExpl,
      json = NULL,
      dynExpl = NULL),
  bad_stations = list(
    stations = ex_bad_stations,
    user_token = ex_user_token,
    dfrom = ex_dfrom,
    dto = ex_dto,
    sensor = ex_sensor,
    staticExpl = ex_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_dfrom = list(
    stations = ex_stations,
    user_token = ex_user_token,
    dfrom = ex_bad_dfrom,
    dto = ex_dto,
    sensor = ex_sensor,
    staticExpl = ex_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_dto = list(
    stations = ex_stations,
    user_token = ex_user_token,
    dfrom = ex_dfrom,
    dto = ex_bad_dto,
    sensor = ex_sensor,
    staticExpl = ex_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_sensor = list(
    stations = ex_stations,
    user_token = ex_user_token,
    dfrom = ex_dfrom,
    dto = ex_dto,
    sensor = ex_bad_sensor,
    staticExpl = ex_staticExpl,
    json = NULL,
    dynExpl = NULL),
  bad_staticExpl = list(
    stations = ex_stations,
    user_token = ex_user_token,
    dfrom = ex_dfrom,
    dto = ex_dto,
    sensor = ex_sensor,
    staticExpl = ex_bad_staticExpl,
    json = NULL,
    dynExpl = NULL)))


#####
## definition of the unit tests

# test1
ex_outputStructure = function(){test_that("Output has the good structure whatever the inputs", {
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
ex_badInput = function(){test_that("Expected behaviour in case of bad parameter", {
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
ex_goodInput = function(){test_that("Expected behaviour in case of good parameters", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "good") {
      for (case in 1:length(group)) {

        object = do.call(what = makeDataset, args = groups[[group]][[case]])

        # the snitch is at TRUE
        expect_true(object$snitch)
        # the returned object at slot value is of class list
        expect_is(object$output$value, class = "list")
        # each element of this list is of class dataframe
        lapply(object$output$value, function(x){expect_is(x, class = "data.frame")})
        # each of these dataframe has at least one row of information
        expect_gte(nrow(object$output$value[[1]]), 1)
        # the colnames of these dataframes all contain the desired sensor and desired explanatory variables
        expect_true(all(c(ex_staticExpl, ex_sensor) %in% unlist(lapply(object$output$value, function(x) {colnames(x)}))))
      }
    }
  }
})}

#####
## execution of the tests. If you want to skip a test, simply comment it :)

ex_outputStructure()
ex_badInput()
ex_goodInput()


