#' @title Generate B bootstrapped cdfs from an input cdf
#'
#' @param cdf A cdf. Ideally the output of projected_dp_cdf. If an unprojected noisy cdf is passed the projection will be done automatically.
#' @param B Number of bootstrap repetitions
#' @return A isotone dp cdf of the data.
#' @export
boot_cdf <-
  function(cdf,
           B = 1000) {

    if(any(base::diff(cdf$y) < 0)) {
      cdf$unprojected_y <- cdf$y
      cdf <- project_dp_cdf(cdf)
    }

    M <- get_M(length(cdf$y))
    L <- fast_factorize_M(length(cdf$y))


    M_inv <- get_inv_M(length(cdf$y))

    # Sample from histogram of cdf
    boot_hist <- rmultinom(B, size = cdf$n, M_inv%*%cdf$y)

    #
    z <- matrix(rnorm(nrow(L)*B, 0, 2 * get_zcdp_sigma(cdf$rho) * sqrt(sum(L[, 1] ^ 2))), nrow = nrow(L))


    boot_noisy_cumsum <- M %*% boot_hist + L %*% z

    boot_noisy_cdf <- boot_noisy_cumsum/cdf$n
    boot_noisy_cdf[boot_noisy_cdf < 0] <- 0
    boot_noisy_cdf[boot_noisy_cdf > 1] <- 1

    cdf$bootstrap_samples <- boot_noisy_cdf

    return(cdf)
  }


