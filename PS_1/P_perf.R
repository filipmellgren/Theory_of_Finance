# Portfolio performance

P_perf <- function(R, w, t){
  # R is a (daily) returns matrix
  # w is a vector of weights
  # t is a time factor for scaling up the daily returns to e.g. yearly or monthly
  R <- as.matrix(R)
  mu <- (1 + mean(R%*%w))^t - 1 # Compounding despite small numbers.
  var <- ( t(w)%*%cov(R)%*%w ) * t
  SR <- mu/sqrt(var)
  
  return(cbind(c("mu", mu), c("Std dev", sqrt(var)), c("Sharpe", SR)))
}