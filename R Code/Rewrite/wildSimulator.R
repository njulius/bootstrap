wildSimulator <- function(numObs, treatRatio, trueTau, numBoots, iterations) {
  
  # Output containers
  
  vHats <- rep(0, times = iterations)
  cvHats <- rep(0, times = iterations)
  fvHats <- rep(0, times = iterations)
  targets <- rep(0, times = iterations)
  
  for(i in 1:iterations) {
    
    # Generate a dataset
    
    Z <- dgpFun(numObs, treatRatio, trueTau)
    
    # Find the matches
    
    matches <- getMatch(Z)
    
    # Set target
    
    targets[i] <- condVar(matches)
    
    # Find ATET
    
    outTHat <- getATET(Z, matches)
    
    # Bootstrap the dataset numBoots times, and save the bHats
    # Construct synthetic correct bHats, save those too
    
    bHats <- rep(0, times = numBoots)
    cbHats <- rep(0, times = numBoots)
    fbHats <- rep(0, times = numBoots)
    
    for(j in 1:numBoots) {
      # Draw rademacher
      radem <- sample(c(-1,1), replace=TRUE, size=numObs)
      
      # Bootstrap the dataset
      
      newZ <- wildBoot(Z, matches, radem)
      
      # Get bHat
      
      bHats[j] <- getATET(newZ, matches)
      
      # Get theory parts
      
      th <- theoryParts(Z, newZ, matches, radem, trueTau)
      
      cbHats[j] <- outTHat + th[2] + th[5]
      fbHats[j] <- outTHat + th[5] # This should (maybe?) be the same for every boot?
    }
    
    # After bootstrapping, get sample variance of both
    # bHats and synthetically correct bHats
    
    vHats[i] <- var(bHats)
    cvHats[i] <- var(cbHats)
    fvHats[i] <- var(fbHats)
    
    cat("Iteration = ", i, "\n", file=stdout())
  }
  
  # After all iterations are run, return variances
  
  out <- cbind(vHats, cvHats, fvHats, targets)
  
  return(out)
}
