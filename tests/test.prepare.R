library(testthat)

test_that('this test fails', {
  expect_that(FALSE, equals(TRUE))
})

test_that("override default variable with user defined value",{
  expect_that(override(NULL, 'default'), equals('default'))
  expect_that(override('user-defined', 'default'), equals('user-defined'))   
})


