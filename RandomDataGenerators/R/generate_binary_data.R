#' Generate Binary Data
#'
#' This function generates random binary data with a specified probability of success.
#'
#' @param n The number of data points to generate.
#' @param prob The probability of success (1). Default is 0.5.
#' @return A numeric vector of 0s and 1s representing binary data.
#' @examples
#' generate_binary_data(100, prob = 0.7)
#' @export
#' @importFrom stats rbinom
generate_binary_data <- function(n, prob = 0.5) {
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  if (!is.numeric(prob) || prob < 0 || prob > 1) {
    stop("Parameter 'prob' must be a numeric value between 0 and 1.")
  }
  
  stats::rbinom(n, size = 1, prob = prob)
}
