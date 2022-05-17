#' @title Wrapper function to generate dp cdf and bootstrapped cdfs from univariate input data x
#'
#' @param x numeric input data
#' @param lower_bound The lower bound of the data
#' @param upper_bound The upper bound of the data
#' @param n Number of observations in original data (if public).
#' @param epsilon Privacy parameter epsilon
#' @param delta Privacy parameter delta
#' @param rho Privacy parameter rho (Either epsilon and delta or rho must be provided.)
#' @param B Number of bootstrap repetitions.
#' @param ... Placeholder for potential additional arguments for the functions called within the wrapper.
#' @return A dp cdf and bootstrap samples
#' @export
dp_bootstrap_cdf <- function(x, lower_bound, upper_bound, n_bins = 1000, excess_bins = TRUE, n = NULL, epsilon, delta, rho = NULL, B = 1000, project = FALSE, ...) {
  binned_data <- bin_data(x, lower_bound, upper_bound, n_bins, excess_bins)

  cdf <- dp_cdf(binned_data, n = n, epsilon = epsilon, delta = delta, rho = rho)

  return(boot_cdf(cdf, B = B, project = project))
}
