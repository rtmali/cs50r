#' Generate Numeric Data
#'
#' This function generates random numeric data with a specified mean and standard deviation.
#'
#' @param n The number of data points to generate. Must be a positive integer.
#' @param mean The mean of the generated data. Default is 0.
#' @param sd The standard deviation of the generated data. Must be non-negative. Default is 1.
#' @return A numeric vector with the generated data.
#' @examples
#' # Generate 1000 data points with a mean of 5 and a standard deviation of 2
#' generate_numeric_data(1000, mean = 5, sd = 2)
#' # Generate 1000 data points with a mean of 10 and a standard deviation of 3
#' generate_numeric_data(1000, mean = 10, sd = 3)
#' @export
#' @importFrom stats rnorm
generate_numeric_data <- function(n, mean = 0, sd = 1) {
  # Validate 'n'
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  # Validate 'sd'
  if (!is.numeric(sd) || sd < 0) {
    stop("Parameter 'sd' must be a non-negative number.")
  }
  
  # Generate random numeric data
  stats::rnorm(n, mean, sd)
}
