# Naive Monte Carlo simulation

doMCSim <- function(iterations, numObs, treatRatio, trueTau, numBoots) {
  
  variances = rep(0, times = iterations)
  targets = rep(0, times = iterations)

  for(i in 1:iterations) {
    # Generate Dataset
    Z <- genData(numObs, treatRatio, trueTau)
    
    # This stores the treatment effect estimates from each bootstrap
    treatEffects = rep(0, times = numBoots)
    matches <- findMatches(Z)
    condVar <- condVar(matches)
    
    for(j in 1:numBoots) {
      # Perform a naive bootstrap
    
      treatedSample <- Z[which(Z[,2] == 1),]
      controlSample <- Z[which(Z[,2] == 0),]
    
      n1 <- length(treatedSample[,1])
      n0 <- length(controlSample[,1])
    
      # Construct bootstrap sample
    
      treatedBootstrap <- treatedSample[sample(nrow(treatedSample), size = n1, replace = TRUE),]
      controlBootstrap <- controlSample[sample(nrow(controlSample), size = n0, replace = TRUE),]
    
      bootedZ <- rbind(treatedBootstrap, controlBootstrap)
    
      treatEffects[j] <- getATE(bootedZ)
      variances[i] <- var(treatEffects)
    }
    
    # We should now have a full vector of treatment effects
    
    # Progress report
    paste("Iteration ", i, " of ", iterations)
    
  }
  
  return(variances)
}
