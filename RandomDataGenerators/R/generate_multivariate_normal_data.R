#' Generate Multivariate Normal Data
#'
#' This function generates random multivariate normal data with specified means and covariance matrix.
#'
#' @param n The number of data points to generate. Must be a positive integer.
#' @param means A numeric vector of means for each variable.
#' @param cov_matrix A covariance matrix for the variables. Must be a square matrix with dimensions matching the length of means.
#' @return A matrix where each row is a multivariate normal data point.
#' @examples
#' means <- c(0, 0)
#' cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
#' generate_multivariate_normal_data(100, means, cov_matrix)
#' @export
generate_multivariate_normal_data <- function(n, means, cov_matrix) {
  # Check if MASS package is installed
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("The MASS package is required but not installed. Please install it using install.packages('MASS').")
  }
  
  # Validate 'n'
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  # Validate 'means'
  if (!is.numeric(means)) {
    stop("Parameter 'means' must be a numeric vector.")
  }
  
  # Validate 'cov_matrix'
  if (!is.matrix(cov_matrix) || nrow(cov_matrix) != ncol(cov_matrix)) {
    stop("Parameter 'cov_matrix' must be a square matrix.")
  }
  
  # Check dimensions
  if (length(means) != nrow(cov_matrix)) {
    stop("The length of 'means' must match the dimensions of 'cov_matrix'.")
  }
  
  # Generate multivariate normal data
  MASS::mvrnorm(n, mu = means, Sigma = cov_matrix)
}
