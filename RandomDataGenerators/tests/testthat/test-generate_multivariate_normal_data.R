# tests/testthat/test-generate_multivariate_normal_data.R

library(testthat)
library(randomDataGenerators)  # Replace with the actual name of your package

test_that("generate_multivariate_normal_data generates correct output", {
  means <- c(0, 0)
  cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  
  # Test for correct dimensions
  result <- generate_multivariate_normal_data(100, means, cov_matrix)
  expect_equal(dim(result), c(100, 2))
  
  # Test for correct mean and covariance (approximately)
  expect_true(abs(mean(result[, 1]) - means[1]) < 0.2)
  expect_true(abs(mean(result[, 2]) - means[2]) < 0.2)
  expect_true(abs(cov(result)[1, 1] - cov_matrix[1, 1]) < 0.2)
  expect_true(abs(cov(result)[2, 2] - cov_matrix[2, 2]) < 0.2)
  expect_true(abs(cov(result)[1, 2] - cov_matrix[1, 2]) < 0.2)
})

test_that("generate_multivariate_normal_data handles invalid inputs", {
  means <- c(0, 0)
  cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  
  # Test for invalid 'n'
  expect_error(generate_multivariate_normal_data(0, means, cov_matrix), "Parameter 'n' must be a positive integer.")
  expect_error(generate_multivariate_normal_data(-1, means, cov_matrix), "Parameter 'n' must be a positive integer.")
  
  # Test for invalid 'means'
  expect_error(generate_multivariate_normal_data(100, "invalid", cov_matrix), "Parameter 'means' must be a numeric vector.")
  
  # Test for invalid 'cov_matrix'
  expect_error(generate_multivariate_normal_data(100, means, "invalid"), "Parameter 'cov_matrix' must be a square matrix.")
  expect_error(generate_multivariate_normal_data(100, means, matrix(c(1, 0.5), nrow = 1)), "Parameter 'cov_matrix' must be a square matrix.")
  
  # Test for mismatched dimensions
  expect_error(generate_multivariate_normal_data(100, c(0, 0, 0), cov_matrix), "The length of 'means' must match the dimensions of 'cov_matrix'.")
})
