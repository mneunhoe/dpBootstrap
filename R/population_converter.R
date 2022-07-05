#' @title Function to run Monte Carlo experiments
#'
#' @param population a dataset with a fixed population or a function to draw n values from a population
#' @return A function to sample n observations from a population
#' @export

population_converter <- function(population) {
  if (is(population, "array")) {
    res <- function(n) {
      population[sample(nrow(population), n, replace = TRUE), ]
    }
  } else if (is(population, "function")) {
    res <- population
  } else if (is(population, "numeric")) {
    res <- function(n) {
      population[sample(length(population), n, replace = TRUE)]
    }
  }

  return(res)
}
