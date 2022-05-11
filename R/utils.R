#' @title Create factorization of sum matrix M
#'
#' @description Provides a function that makes it easy to sample synthetic data from a Generator
#'
#' @param n The length of the vector to privatize
#' @return Matrix L (and/or R) where L%*%R = M
#' @export
fast_factorize_M <- function(n) {
  pre_calc <- base::exp(base::cumsum(base::log((1:n) * 2 - 1)) - base::cumsum(base::log((1:n) * 2)))

  L <- base::diag(1, nrow = n)
  for (i in 1:(nrow(L) - 1)) {
    L[(i + 1):nrow(L), i] <- pre_calc[1:(nrow(L) - i)]
  }

  return(L)
}

#' @title Calculate the rho of zCDP based on epsilon and delta
#'
#' @param epsilon The epsilon
#' @param delta The delta
#' @return rho
#' @export
get_zcdp_rho <- function(epsilon, delta) {
  epsilon + 2 * base::log(1 / delta) - 2 * base::sqrt(epsilon * base::log(1 / delta) + (base::log(1 /
                                                                            delta)) ^ 2)
}

#' @title Calculate the sigma (standard deviation) for the Gaussian mechanism to achieve rho-zCDP
#'
#' @param rho The privacy parameter rho
#' @param sensitivity The sensitivity of the query
#' @return rho
#' @export
get_zcdp_sigma <- function(rho, sensitivity = 1) {
  sensitivity / base::sqrt(2 * rho)
}

#' @title Create query matrix M for cumulative (prefix) sum queries
#'
#' @param n The length of the vector to privatize
#' @return Query matrix M
#' @export
get_M <- function(n) {
  M <- base::diag(1, nrow = n)
  M[base::lower.tri(M, diag = FALSE)] <- 1

  return(M)
}

#' @title Create inverse of query matrix M for cumulative (prefix) sum queries
#'
#' @param n The length of the vector to privatize
#' @return Inverse of query matrix M
#' @export
get_inv_M <- function(n) {
  inv_M <- base::diag(1, nrow = n)
  for (i in 1:(nrow(inv_M) - 1)) {
    inv_M[i + 1, i] <- -1
  }

  return(inv_M)
}
