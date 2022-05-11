#' @title Generate histogram of the input vector
#'
#' @param x The privacy parameter rho
#' @param lower_bound The lower bound of the data
#' @param upper_bound The upper bound of the data
#' @param n_bins The number of bins. (Default: 1000)
#' @param excess_bins Do you want to include excess bins from -Inf to lower_bound and upper_bound to Inf? (Default: TRUE)
#' @return Histogram of the data with corresponding x values.
#' @export
bin_data <-
  function(x,
           lower_bound,
           upper_bound,
           n_bins = 1000,
           excess_bins = TRUE) {
    #
    breaks <-
      seq(lower_bound,
          upper_bound,
          length.out = ifelse(excess_bins, n_bins - 1, n_bins + 1))

    if (excess_bins) {
      # Add low and high excess bins
      breaks <- c(-Inf, breaks, Inf)
    }
    # Generate counts per bin
    true_hist <- .Call(graphics:::C_BinCount, x, breaks, TRUE, TRUE)

    return(list(breaks = breaks, histogram = true_hist))
  }
