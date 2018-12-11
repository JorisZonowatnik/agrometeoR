# testthat::test_file("./tests/testthat/test_spatial_gstat.R")
# devtools::test(filter = "gstat")
# devtools::document()
# devtools::load_all()
# devtools::test(filter = "spatial_regr_gstat")
# https://stackoverflow.com/questions/31548796/debugging-testthat-tests-in-rstudio
# sink(NULL)
# https://github.com/r-lib/devtools/issues/1675 - Error in x[[method]](...) : attempt to apply non-function
# https://stackoverflow.com/questions/50083521/error-in-xmethod-attempt-to-apply-non-function-in-testthat-test-when
# https://stackoverflow.com/questions/7028385/can-i-remove-an-element-in-dot-dot-dot-and-pass-it-on
# http://r-pkgs.had.co.nz/tests.html

context("makeDataset")
devtools::load_all()

# list(bool = bool, output = output))
test_that("output has the good structure", {
  expect_is(object = makeDataset(), class = "list")
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})

test_that("str_length of factor is length of level", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})

test_that("str_length of missing is missing", {
  expect_equal(str_length(NA), NA_integer_)
  expect_equal(str_length(c(NA, 1)), c(NA, 1))
  expect_equal(str_length("NA"), 2)
})
