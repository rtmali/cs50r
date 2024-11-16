# tests/testthat/test-generate_categorical_data.R

library(testthat)
library(randomDataGenerators)  # Replace with your actual package name

test_that("generate_categorical_data works with decimal proportions", {
  set.seed(123)
  result <- generate_categorical_data(100, c("A", "B"), c(0.5, 0.5))
  expect_equal(length(result), 100)
  expect_true(all(result %in% c("A", "B")))
})

test_that("generate_categorical_data works with percentage proportions", {
  set.seed(123)
  result <- generate_categorical_data(100, c("A", "B"), c(50, 50))
  expect_equal(length(result), 100)
  expect_true(all(result %in% c("A", "B")))
})

test_that("generate_categorical_data handles unequal proportions", {
  set.seed(123)
  result <- generate_categorical_data(100, c("A", "B", "C"), c(0.2, 0.3, 0.5))
  expect_equal(length(result), 100)
  expect_true(all(result %in% c("A", "B", "C")))
})

test_that("generate_categorical_data handles unequal percentage proportions", {
  set.seed(123)
  result <- generate_categorical_data(100, c("A", "B", "C"), c(20, 30, 50))
  expect_equal(length(result), 100)
  expect_true(all(result %in% c("A", "B", "C")))
})

test_that("generate_categorical_data errors on incorrect proportions", {
  expect_error(generate_categorical_data(100, c("A", "B"), c(0.5, 0.6)), 
               "Proportions must sum to 1 if using decimals, or 100 if using percentages.")
  expect_error(generate_categorical_data(100, c("A", "B"), c(50, 60)), 
               "Proportions must sum to 1 if using decimals, or 100 if using percentages.")
})
