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
  sd.asset1 <- sqrt(var(diff(asset1))*252/t)
  sd.asset2 <- sqrt(var(diff(asset2))*252/t)
  
  mu <- c(r.asset1, r.asset2)
  mustar <- rep(550:700, length.out = 1000)/1000
  # Covariance
  cov <- rho * sd.asset1 * sd.asset2
  Sigma <- rbind(c(sd.asset1^2, cov), c(cov, sd.asset2^2))
  w <- MVcalc(mustar, mu, Sigma)
  
  
  for (ix in 1:length(mustar)) {
    sigma[ix] <- sqrt((w[ix])^2 * sd.asset1^2 + (1-w[ix])^2 * sd.asset2^2 + 2*w[ix]*(1-w[ix])*cov)
  }
  
  
  df.MVF <- as_tibble(cbind(w, mustar, sigma))
  
  return(df.MVF)
}