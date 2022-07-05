#' @title Function to run Monte Carlo experiments
#'
#' @param population a dataset with a fixed population or a function to draw n values from a population
#' @param n The sample size
#' @param mechanism dp mechanism. A function that takes a data set x as input and produces a generative model.
#' @param statistics_of_interest Functions to calculate the statistics of interest on the output of the mechanism, collected in a list
#' @param B number of bootstrap repetitions
#' @param store_P_tilde Do you want to store the initial generative model? Default: FALSE
#' @param store_P_tilde_tilde Do you want to sstore all bootstrapped generative models? Default: FALSE
#' @return A list with the calculated statistics of interest and their bootstrap distribution
#' @export

run_experiment <- function(run_id = "testrun",
                           results_directory = NULL,
                           population,
                           n,
                           mechanism,
                           ...,
                           get_synthetic_data,
                           statistics_of_interest,
                           B = 1000,
                           store_P_tilde = FALSE,
                           store_P_tilde_tilde = FALSE
                           ) {

  drawn_sample <- population_converter(population)(n)
  instantiated_mechanism <- function(data) {
    mechanism(data, ...)
  }

 result <-  dp_bootstrap(
    data = drawn_sample,
    mechanism = instantiated_mechanism,
    get_synthetic_data = get_synthetic_data,
    statistics_of_interest = statistics_of_interest,
    B = B,
    store_P_tilde = store_P_tilde,
    store_P_tilde_tilde = store_P_tilde_tilde
  )

if(is.null(results_directory)) {
  dir.create(here::here("experiments"))
  path <- here::here(paste0("experiments/", run_id, ".RDS"))
  } else {
    path <- paste0(results_directory,"/", run_id, ".RDS")
  }
saveRDS(result, path)
}




