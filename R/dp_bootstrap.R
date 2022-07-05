#' @title Generic pipeline for dp bootstrap
#'
#' @param data input dataset
#' @param mechanism dp mechanism. A function that takes a data set x as input and produces a generative model.
#' @param statistics_of_interest Functions to calculate the statistics of interest on the output of the mechanism, collected in a list
#' @param B number of bootstrap repetitions
#' @param store_P_tilde Do you want to store the initial generative model? Default: FALSE
#' @param store_P_tilde_tilde Do you want to store all bootstrapped generative models? Default: FALSE
#' @return A list with the calculated statistics of interest and their bootstrap distribution
#' @export
dp_bootstrap <-
  function(data,
           mechanism,
           get_synthetic_data,
           statistics_of_interest,
           B = 1000,
           store_P_tilde = FALSE,
           store_P_tilde_tilde = FALSE
           ) {
    P_tilde <- mechanism(data)
    theta_tilde <- statistics_of_interest(P_tilde)

    # Initialize results object
    theta_tilde_tilde_list <- vector("list", length = B)

    if(store_P_tilde_tilde) {
      P_tilde_tilde_list <-  vector("list", length = B)
    }

    # Progress bar
    cli::cli_progress_bar("Bootstrapping", total = B)
    for (b in 1:B) {
      # Get synthetic data
      synthetic_data <- get_synthetic_data(P_tilde)

      # Run mechanism
      P_tilde_tilde <- mechanism(synthetic_data)

      if(store_P_tilde_tilde) {
        P_tilde_tilde_list[[b]] <- P_tilde_tilde
      }

      # Calculate statistics
      theta_tilde_tilde <- statistics_of_interest(P_tilde_tilde)

      # Store statistics
      theta_tilde_tilde_list[[b]] <- theta_tilde_tilde

      # Update Progress bar
      cli::cli_progress_update()
    }

    res <- list(
      theta_tilde = theta_tilde,
      theta_tilde_tilde = theta_tilde_tilde_list,
      P_tilde = if(store_P_tilde) P_tilde else NULL,
      P_tilde_tilde = if(store_P_tilde_tilde) P_tilde_tilde_list else NULL
    )
    return(
      res
    )
  }
