#' @title Generic pipeline for dp bootstrap
#'
#' @param x input data
#' @param mechanism dp mechanism. A function that takes x as input and produces "synthetic data".
#' @param statistics_of_interest Functions to calculate on the output of the mechanism, collected in a list
#' @param bootstrap_loop A function of how to sample bootstrap samples from the output of the mechanism and loop over them
#' @param B number of bootstrap repetitions
#' @return A list with the calculated statistics of interest and their bootstrap distribution
#' @export
dp_bootstrap <-
  function(x,
           mechanism,
           statistics_of_interest,
           B = 1000,
           bootstrap_loop) {
    x_tilde <- mechanism(x)
    theta_tilde <- statistics_of_interest(x_tilde)

    bootstrap_results <-
      bootstrap_loop(x_tilde,
                     mechanism,
                     statistics_of_interest,
                     B)


    return(return(
      list(theta_tilde = theta_tilde,
           theta_tilde_tilde = bootstrap_results)
    ))
  }
