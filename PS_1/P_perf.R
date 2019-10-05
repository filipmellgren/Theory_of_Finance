# Portfolio performance
# Filip Mellgren
# Sep 25th 2019

P_perf <- function(R, w, t, rf){
  # R is a (daily) returns matrix
  # w is a vector of weights
  # t is a time factor for scaling up the daily returns to e.g. yearly or monthly
  # rf is the risk free rate
  
  # Note that returns are compounded! 
  # This results in the Sharpe ratio not growing with the square root of time.
  
  R <- as.matrix(R)
  mu <- (1 + mean(R%*%w))^t - 1 
  rf <- (1 + rf)^(t/252)-1 # Make daily by dividing with n.o. trading days
  mu_e <-  mu - rf  
  var <- ( t(w)%*%cov(R)%*%w ) * t
  SR <- mu_e/sqrt(var)
  
  return(cbind(c("mu", mu), c("Std dev", sqrt(var)), c("Sharpe", SR)))
}