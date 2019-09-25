# Mean variance frontier
# Filip Mellgren
# Sep 25th 2019

MVF <- function(asset1, asset2, rho){
  # asset1 is daily observed values of an asset
  # asset2 is analogous to asset1
  # rho is an arbitrarily imposed correlation between the two assets
  
  # Annual returns calculated as return so far over a time period t
  # scaled by to be that of a year
  
  t <- length(asset1)
  r.asset1 <- exp((asset1[t]/asset1[1] - 1) * 252/t) -1
  r.asset2 <- exp((asset2[t]/asset2[1] - 1) * 252/t) -1
  
  # Annualised volatility
  sd.asset1 <- sqrt(var(asset1)*252/t)
  sd.asset2 <- sqrt(var(asset2)*252/t)
  
  # Weight on asset1 vector, allows for short selling
  w <- (-200:200)/200
  
  # Portfolio average return
  mu <- w*r.asset1 + (1-w)*r.asset2
  
  # Covariance
  cov <- rho * sd.asset1 * sd.asset2
  
  sigma <- sqrt(w^2 * sd.asset1^2 + (1-w)^2 * sd.asset2^2 + 2*w*(1-w)*cov)
  
  df.MVF <- as_tibble(cbind(w, mu, sigma))
  
  return(df.MVF)
}