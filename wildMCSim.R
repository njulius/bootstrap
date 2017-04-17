wildMCSim <- function(iterations, numObs, treatRatio, trueTau, numBoots) {
  
  variances = rep(0, times = iterations)
  targets = rep(0, times = iterations)
  
  for(i in 1:iterations) {
    # Generate Dataset
    Z <- genData(numObs, treatRatio, trueTau)
    
    # This stores the treatment effect estimates from each bootstrap
    treatEffects = rep(0, times = numBoots)
    
    # Get matching matrix
    matches <- findMatches(Z)
    targets[i] <- condVar(matches)
    
    for(j in 1:numBoots) {
      # Perform a wild
      
      newZ <- wildBootstrap(Z, matches)
      
      # Get bootstrapped estimator
      treatEffects[j] <- getATE(newZ)
    }
    
    variances[i] <- var(treatEffects)
    
    # We should now have a full vector of treatment effects
    
    # Progress report
    paste("Iteration ", i, " of ", iterations)
    
  }
  
  return(variances)
}
