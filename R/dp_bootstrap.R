#' @title Generic pipeline for dp bootstrap
#'
#' @param x input data
#' @param mechanism dp mechanism. A function that takes x as input and produces "synthetic data".
#' @param statistics_of_interest Functions to calculate on the output of the mechanism, collected in a list
#' @param bootstrap_sampler A function of how to sample bootstrap samples from the output of the mechanism.
#' @param B number of bootstrap repetitions
#' @return A list with the calculated statistics of interest and their bootstrap distribution
#' @export
dp_bootstrap <-
  function(x,
           mechanism,
           statistics_of_interest,
           B = 1000,
           one_bootstrap_sample,
           bootstrap_sampler,
           bootstrap_loop) {
    x_tilde <- mechanism(x)
    theta_tilde <- statistics_of_interest(x_tilde)

    bootstrap_ids_list <- bootstrap_sampler(x_tilde, B)

    bootstrap_results <-
      bootstrap_loop(x_tilde,
                     bootstrap_ids_list,
                     mechanism,
                     statistics_of_interest)


    return(return(
      list(theta_tilde = theta_tilde,
           theta_tilde_tilde = bootstrap_results)
    ))
  }
