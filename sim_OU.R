#' @Purpose: To simulate time-homogeneous Ornstein–Uhlenbeck process
#' 
#' @param
#' N: the number of output replicated curves
#' Nt: the number of equal-spaced discretized points. 

sim_OU <- function(N, Nt){
  sig <- 1
  s <- seq(0, 1, length.out = Nt)
  EXt <- rep(0, length(s))
  CovXt <- matrix(NA, Nt, Nt)
  for(i in 1:Nt){
    for(j in 1:Nt){
      CovXt[i, j] <- sig^2 * exp(-0.5*s[i])*exp(-0.5*s[j])*min(exp(s[i]), exp(s[j]))
    }
  }
  return(t(MASS::mvrnorm(n = N, mu = EXt, Sigma = CovXt)))
}