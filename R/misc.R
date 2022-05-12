cdfFancyPostProcess <-
  function(vals, cdf, a_list_lower, a_list_upper) {
    # for lower interval, look at upper cis
    distances = cdf - a_list_lower
    indices = which(distances >= 0)
    if (length(indices) > 0 & min(indices) > 1) {
      i = min(indices) - 1
    } else {
      i = 1
    }
    lower_val = vals[i]

    # vice versa
    distances = cdf - a_list_upper
    indices = which(distances <= 0)
    if (length(indices) > 0 & max(indices) < length(vals) - 1) {
      i = max(indices) + 1
    } else{
      i = length(vals)
    }
    # print("upper measurement", cdf[i], "corresponding a", a_list_upper[i])
    upper_val = vals[i]

    return(c(lower_val, upper_val))
  }


preProcessCDF <-
  function(n,
           lower_bound,
           upper_bound,
           granularity,
           epsilon,
           alpha,
           cdp = TRUE,
           variances = NULL) {
    if (is.null(variances)) {
      x = seq(lower_bound, upper_bound, length.out = n)
      n_bins = (upper_bound - lower_bound) %/% granularity + 1
      depth = as.integer(log2(n_bins) %/% 1)
      tt = dpTree(x, length(x), lower_bound, upper_bound, epsilon, depth, cdp =
                    cdp)
      tOpt = optimalPostProcess(tt, epsilon)[[1]]

      variances = getVariances(tOpt, epsilon, cdp = cdp)
    }
    # Compute all the binomial/normal approx coefficients
    qm_probs = dbinom(0:n, n, 0.5)

    # Compute the a_is
    a_lower = 0.0
    a_upper = 1.0
    a_list_lower = NULL
    a_list_upper = NULL
    for (i in 1:length(variances)) {
      noise_scale = sqrt(variances[i]) / (n) # variances are for counts, we want std for percents
      # print("noise_scale:", noise_scale)
      a_i_upper = a_binsearch(n,
                              qm_probs,
                              noise_scale,
                              a_lower,
                              a_upper,
                              alpha / 2.0,
                              epsilon)

      a_list_upper <- c(a_list_upper, a_i_upper)
      a_list_lower <- c(a_list_lower, 1.0 - a_i_upper)
    }
    cat("preprocessing finished!\n")
    return(list(a_list_lower, a_list_upper, variances))
  }


get_quantile <- function(cdf,
                         q,
                         cdf_x = NULL,
                         cdf_y = NULL) {
  if (is.null(cdf_x) & is.null(cdf_y)) {
    cdf_x <- cdf[[2]]
    cdf_y <- cdf[[1]]
  }

  min(cdf_x[cdf_y >= q])
}
