#' Generate Poisson Data
#'
#' This function generates random data following a Poisson distribution with a specified lambda.
#'
#' @param n The number of data points to generate. Must be a positive integer.
#' @param lambda The rate parameter (lambda) of the Poisson distribution. Must be non-negative. Default is 1.
#' @return A numeric vector representing Poisson-distributed data.
#' @examples
#' generate_poisson_data(100, lambda = 3)
#' @export
generate_poisson_data <- function(n, lambda = 1) {
  # Validate 'n'
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  # Validate 'lambda'
  if (!is.numeric(lambda) || lambda < 0) {
    stop("Parameter 'lambda' must be a non-negative number.")
  }
  
  # Generate Poisson-distributed data
  stats::rpois(n, lambda)
}
