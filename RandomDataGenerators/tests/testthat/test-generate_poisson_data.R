# tests/testthat/test-generate_poisson_data.R
# This file contains tests for the generate_poisson_data function.

library(testthat)
library(randomDataGenerators)  # Replace with your actual package name

test_that("generate_poisson_data generates correct number of observations", {
  result <- generate_poisson_data(100)
  expect_equal(length(result), 100)
})

test_that("generate_poisson_data generates data with approximately correct lambda", {
  set.seed(123) # Set seed for reproducibility
  n <- 1000
  lambda <- 3
  result <- generate_poisson_data(n, lambda = lambda)
  
  # Check if the mean is approximately equal to lambda
  expect_true(abs(mean(result) - lambda) < 0.1)
})

test_that("generate_poisson_data handles edge cases", {
  # Test with n = 0 should expect an error
  expect_error(generate_poisson_data(0), "Parameter 'n' must be a positive integer.")
  
  # Test with lambda = 0
  result <- generate_poisson_data(100, lambda = 0)
  expect_true(all(result == 0))
})

test_that("generate_poisson_data handles default parameters", {
  set.seed(123)
  result <- generate_poisson_data(100)
  
  # Check if the mean of the result is approximately 1 (default lambda)
  expect_true(abs(mean(result) - 1) < 0.1)
})
