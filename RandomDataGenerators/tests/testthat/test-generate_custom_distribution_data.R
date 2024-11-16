# tests/testthat/test-generate_custom_distribution_data.R
# This file contains tests for the generate_custom_distribution_data function.

library(testthat)
library(randomDataGenerators)  # Replace with your actual package name

# Define a custom PDF for testing
custom_pdf <- function(x) {
  ifelse(x >= 0 & x <= 1, 2 * x, 0)
}

# Test cases for generate_custom_distribution_data
test_that("generate_custom_distribution_data works correctly", {
  # Test that the function returns a numeric vector of the correct length
  n <- 100
  data <- generate_custom_distribution_data(n, custom_pdf, 0, 1)
  expect_type(data, "double")
  expect_length(data, n)
  
  # Test that the generated data is within the specified bounds
  expect_true(all(data >= 0 & data <= 1))
  
  # Test with a different PDF
  custom_pdf2 <- function(x) {
    ifelse(x >= -1 & x <= 1, 1 - abs(x), 0)
  }
  data2 <- generate_custom_distribution_data(n, custom_pdf2, -1, 1)
  expect_type(data2, "double")
  expect_length(data2, n)
  expect_true(all(data2 >= -1 & data2 <= 1))
  
  # Test edge case with n = 0
  expect_error(generate_custom_distribution_data(0, custom_pdf, 0, 1), 
               "Parameter 'n' must be a positive integer.")
  
  # Test edge case with invalid PDF (all zero probabilities)
  custom_pdf_invalid <- function(x) {
    rep(0, length(x))
  }
  expect_error(generate_custom_distribution_data(n, custom_pdf_invalid, 0, 1), 
               "The sum of probabilities is zero. Please check the PDF function.")
})
