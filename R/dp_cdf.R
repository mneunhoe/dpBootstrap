#' @title Generate differentially private cdf from histogram
#'
#' @param binned_data The output of bin_data
#' @param n The number of observations in the original data (if public, otherwise the max of the noisy cumsum will be used). Default: NULL (i.e. use maximum of noisy cumsum).
#' @param epsilon Privacy parameter epsilon
#' @param delta Privacy parameter delta
#' @param rho Privacy parameter rho. Either epsilon and delta or rho must be provided. Rho overwrites any entries in epsilon and delta.
#' @return A noisy cdf of the data.
#' @export
dp_cdf <-
  function(binned_data,
           n = NULL,
           epsilon,
           delta,
           rho = NULL) {
    if (is.null(rho)) {
      rho <-
        epsilon + 2 * log(1 / delta) - 2 * sqrt(epsilon * log(1 / delta) + (log(1 /
                                                                                  delta)) ^ 2)
    }

    histogram <- binned_data$histogram
    # Get query matrix
    M <- get_M(length(histogram))

    # Get factorization of query matrix
    L <- fast_factorize_M(length(histogram))

    # Draw noise vector for rho-zCDP
    z <-
      rnorm(nrow(L), 0, get_zcdp_sigma(rho) * sqrt(sum(L[, 1] ^ 2)))

    # Calculate noisy cumulative sum of histogram
    noisy_cumsum <- M %*% histogram + L %*% z

    # Get n (if not provided)
    if (is.null(n)) {
      n <- base::max(noisy_cumsum)
    }

    noisy_cdf <-
      c(0, (noisy_cumsum / n)[1:length(noisy_cumsum) - 1, 1], 1)

    noisy_cdf[noisy_cdf < 0] <- 0
    noisy_cdf[noisy_cdf > 1] <- 1

    return(list(
      y = noisy_cdf,
      x = binned_data$breaks,
      rho = rho,
      n = n
    ))
  }
