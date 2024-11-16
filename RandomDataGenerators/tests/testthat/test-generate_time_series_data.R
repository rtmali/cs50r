# tests/testthat/test-generate_time_series_data.R

library(testthat)
library(randomDataGenerators)  # Replace with your actual package name

test_that("generate_time_series_data generates correct number of observations", {
  result <- generate_time_series_data(100)
  expect_equal(length(result), 100)
})

test_that("generate_time_series_data generates data with correct trend", {
  set.seed(123) # Set seed for reproducibility
  n <- 100
  trend <- 0.5
  result <- generate_time_series_data(n, trend = trend, noise_sd = 0)
  
  # Expected result is a linear trend with no noise
  expected <- trend * (1:n)
  expect_equal(result, expected)
})

test_that("generate_time_series_data includes noise component", {
  set.seed(123) # Set seed for reproducibility
  n <- 100
  trend <- 0
  noise_sd <- 1
  result <- generate_time_series_data(n, trend = trend, noise_sd = noise_sd)
  
  # Check if the standard deviation of the result is approximately equal to noise_sd
  expect_true(abs(sd(result) - noise_sd) < 0.1)
})

test_that("generate_time_series_data handles edge cases", {
  # Test with n = 0 should expect an error if your function is designed to throw one
  expect_error(generate_time_series_data(0), "Parameter 'n' must be a positive integer.")
  
  # Test with n = 1
  result <- generate_time_series_data(1, trend = 1, noise_sd = 0)
  expect_equal(result, 1)
})

test_that("generate_time_series_data handles default parameters", {
  set.seed(123)
  result <- generate_time_series_data(100)
  
  # Check if the mean of the result is approximately 0
  expect_true(abs(mean(result)) < 0.1)
  
  # Check if the standard deviation is approximately 1
  expect_true(abs(sd(result) - 1) < 0.1)
})
