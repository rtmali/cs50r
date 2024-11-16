#' Generate Time Series Data
#'
#' This function generates random time series data with a specified linear trend and noise level.
#'
#' @param n The number of data points to generate. Must be a positive integer.
#' @param trend The linear trend component to be added to the time series. Default is 0.
#' @param noise_sd The standard deviation of the noise component. Must be non-negative. Default is 1.
#' @return A numeric vector representing the generated time series data.
#' @examples
#' # Generate 100 data points with a trend of 0.5 and noise standard deviation of 1
#' generate_time_series_data(100, trend = 0.5, noise_sd = 1)
#' # Generate 100 data points with a trend of 1 and noise standard deviation of 2
#' generate_time_series_data(100, trend = 1, noise_sd = 2)
#' @export
generate_time_series_data <- function(n, trend = 0, noise_sd = 1) {
  # Validate 'n'
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  # Validate 'noise_sd'
  if (!is.numeric(noise_sd) || noise_sd < 0) {
    stop("Parameter 'noise_sd' must be a non-negative number.")
  }
  
  # Generate time series data
  time <- 1:n
  trend_component <- trend * time
  noise_component <- stats::rnorm(n, mean = 0, sd = noise_sd)
  trend_component + noise_component
}
