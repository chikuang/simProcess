#' @Purpose: To simulate the proceess which coefficients follows N(0,1),
#'  and the basis are D-dimensional Fourier basis
#' 
#' @param
#' N: the number of output replicated curves
#' Nt: the number of equal-spaced discretized points. 

sim_fourier <- function(N, Nt, D){
  s <- seq(0, 1, length.out = Nt)
  sapply(1:N, function(i){
    Z <- rnorm(D)
    fda::fourier(s, nbasis = D) %*% Z
  }) |> t()
}