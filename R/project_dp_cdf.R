#' @title Project noisy cdf to isotone cdf
#'
#' @param dp_cdf The output of dp_cdf
#' @return A isotone dp cdf of the data.
#' @export
project_dp_cdf <-
  function(dp_cdf) {
    dp_cdf$y <- isotone::gpava(dp_cdf$x, dp_cdf$y)$x
    return(dp_cdf)
  }
