#' @Purpose: To simulate the proceess which coefficients follows N(0,1),
#'  and the basis are D-dimensional Bspline basis
#' 
#' @param
#' N: the number of output replicated curves
#' Nt: the number of equal-spaced discretized points. 

sim_Bspline <- function(N, Nt, D){
  s <- seq(0, 1, length.out = Nt)
  sapply(1:N, function(i){
    bet <- rbeta(D, 2.5, 5)
    bs <- eval.basis(s, create.bspline.basis(c(0, 1), nbasis = D))
    bs %*% bet
  }) |> t()
}