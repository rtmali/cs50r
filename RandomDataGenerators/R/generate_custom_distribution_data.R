#' Generate Custom Distribution Data
#'
#' This function generates random data from a custom distribution defined by a probability density function.
#'
#' @param n The number of data points to generate.
#' @param pdf A function representing the probability density function of the custom distribution.
#' @param lower The lower bound of the distribution.
#' @param upper The upper bound of the distribution.
#' @return A numeric vector representing data from the custom distribution.
#' @examples
#' custom_pdf <- function(x) { ifelse(x >= 0 & x <= 1, 2 * x, 0) }
#' generate_custom_distribution_data(100, custom_pdf, 0, 1)
#' @export
generate_custom_distribution_data <- function(n, pdf, lower, upper) {
  # Validate 'n'
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  # Validate 'lower' and 'upper'
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper) {
    stop("Parameters 'lower' and 'upper' must be numeric, with 'lower' less than 'upper'.")
  }
  
  # Validate 'pdf'
  if (!is.function(pdf)) {
    stop("Parameter 'pdf' must be a function.")
  }
  
  # Create a sample space
  sample_space <- seq(lower, upper, length.out = 10000)
  
  # Calculate probabilities using the provided PDF
  probabilities <- pdf(sample_space)
  
  # Ensure probabilities are non-negative
  if (any(probabilities < 0)) {
    stop("PDF function must return non-negative values.")
  }
  
  # Handle NA values in probabilities
  probabilities[is.na(probabilities)] <- 0
  
  # Check if the sum of probabilities is zero
  total_prob <- sum(probabilities)
  if (total_prob == 0) {
    stop("The sum of probabilities is zero. Please check the PDF function.")
  }
  
  # Normalize the probabilities
  probabilities <- probabilities / total_prob
  
  # Generate samples
  sample(sample_space, size = n, replace = TRUE, prob = probabilities)
}
