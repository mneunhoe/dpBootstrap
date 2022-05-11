#' @title Wrapper function to generate dp cdf and bootstrapped cdfs from univariate input data x
#'
#' @param x numeric input data
#' @param lower_bound
#' @param upper_bound
#' @param n Number of observations in original data (if public).
#' @param epsilon
#' @param delta
#' @param rho
#' @param B Number of bootstrap repetitions.
#' @param ... Placeholder for potential additional arguments for the functions called within the wrapper.
#' @return A dp cdf and bootstrap samples
#' @export
dp_bootstrap_cdf <- function(x, lower_bound, upper_bound, n = NULL, epsilon, delta, rho = NULL, B = 1000, ...) {
  binned_data <- bin_data(x, lower_bound, upper_bound, ...)

  cdf <- dp_cdf(binned_data, n = n, epsilon = epsilon, delta = delta, rho = rho)

  return(boot_cdf(cdf, B = B))
}
