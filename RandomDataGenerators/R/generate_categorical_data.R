#' Generate Categorical Data
#'
#' This function creates random categorical data with specified levels and proportions.
#' Proportions can be specified as decimal fractions (e.g., 0.2, 0.5) or percentages (e.g., 20, 50).
#'
#' @param n The number of data points to generate.
#' @param levels A vector of category names (e.g., c("A", "B", "C")).
#' @param proportions A vector of proportions for each category. Must sum to 1 if using decimals, or 100 if using percentages.
#' @return A factor vector with the generated categorical data.
#' @examples
#' # Using decimal proportions
#' generate_categorical_data(100, c("A", "B", "C"), c(0.2, 0.3, 0.5))
#' # Using percentage proportions
#' generate_categorical_data(100, c("A", "B", "C"), c(20, 30, 50))
#' @export
generate_categorical_data <- function(n, levels, proportions) {
  # Validate 'n'
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  # Validate 'levels' and 'proportions'
  if (length(levels) != length(proportions) || length(levels) == 0) {
    stop("Levels and proportions must have the same non-zero length.")
  }
  
  # Check if proportions are specified as percentages
  if (sum(proportions) > 1) {
    proportions <- proportions / 100
  }
  
  # Validate proportions sum
  if (abs(sum(proportions) - 1) > .Machine$double.eps^0.5) {
    stop("Proportions must sum to 1 if using decimals, or 100 if using percentages.")
  }
  
  # Generate categorical data
  factor(sample(levels, size = n, replace = TRUE, prob = proportions), levels = levels)
}
