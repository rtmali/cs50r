# tests/testthat/test-generate_numeric_data.R

library(testthat)
library(randomDataGenerators)  # Replace with your actual package name

test_that("generate_numeric_data generates correct number of observations", {
  result <- generate_numeric_data(100)
  expect_equal(length(result), 100)
})

test_that("generate_numeric_data generates data with approximately correct mean and sd", {
  set.seed(123) # Set seed for reproducibility
  result <- generate_numeric_data(1000, mean = 5, sd = 2)
  
  # Check if the mean is approximately 5
  expect_true(abs(mean(result) - 5) < 0.1)
  
  # Check if the standard deviation is approximately 2
  expect_true(abs(sd(result) - 2) < 0.1)
})

test_that("generate_numeric_data handles edge cases", {
  # Test with n = 0 should expect an error if your function is designed to throw one
  expect_error(generate_numeric_data(0), "Parameter 'n' must be a positive integer.")
  
  # Test with n = 1
  result <- generate_numeric_data(1, mean = 10, sd = 0)
  expect_equal(result, 10)
})

test_that("generate_numeric_data handles default parameters", {
  set.seed(123)
  result <- generate_numeric_data(100)
  
  # Check if the mean is approximately 0
  expect_true(abs(mean(result)) < 0.1)
  
  # Check if the standard deviation is approximately 1
  expect_true(abs(sd(result) - 1) < 0.1)
})
