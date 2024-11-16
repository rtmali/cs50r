# tests/testthat/test-generate_binary_data.R

library(testthat)
library(randomDataGenerators)  # Replace with your actual package name

test_that("generate_binary_data generates correct number of observations", {
  result <- generate_binary_data(100)
  expect_equal(length(result), 100)
})

test_that("generate_binary_data generates data with approximately correct probability", {
  set.seed(123) # Set seed for reproducibility
  n <- 1000
  prob <- 0.7
  result <- generate_binary_data(n, prob = prob)
  
  # Check if the proportion of 1s is approximately equal to prob
  proportion_of_ones <- mean(result)
  expect_true(abs(proportion_of_ones - prob) < 0.05)
})

test_that("generate_binary_data handles edge cases", {
  # Test with n = 0 should expect an error
  expect_error(generate_binary_data(0), "Parameter 'n' must be a positive integer.")
  
  # Test with n = 1
  result <- generate_binary_data(1, prob = 0.8)
  expect_true(result %in% c(0, 1))
})

test_that("generate_binary_data handles default parameters", {
  set.seed(123)
  result <- generate_binary_data(100)
  
  # Check if the proportion of 1s is approximately 0.5
  proportion_of_ones <- mean(result)
  expect_true(abs(proportion_of_ones - 0.5) < 0.1)
})
