wildSimulatorATT <- function(nTreated, nControl, trueTau, numBoots, iterations) {
  
  # Output containers
  
  vHats <- rep(0, times = iterations)
  cvHats <- rep(0, times = iterations)
  targets <- rep(0, times = iterations)
  
  for(i in 1:iterations) {
    
    # Generate dataset
    
    Z <- initiatorATT(nTreated, nControl, trueTau)
    
    # Conditional Variance is just n1^(-2) * sum(k^2)
    
    kSq <- Z[,5]^2
    
    targets[i] <- (nTreated)^(-2) * sum(kSq)
    
    # Get ATT
    
    tHat <- mean(Z[,6], na.rm=TRUE)
    
    # Bootstrap containers
    
    bHats <- rep(0, times = numBoots)
    cbHats <- rep(0, times = numBoots)
    
    for(j in 1:numBoots) {
      
      # Draw rademacher
      
      radem <- sample(c(-1,1), replace=TRUE, size=(nTreated+nControl))
      
      # Bootstrap the dataset
      
      newZ <- wildBootATT(Z, radem)
      
      # Get bHat
      
      bHats[j] <- mean(newZ[,6], na.rm=TRUE)
      
      # Get theory parts
      
      th <- theoryPartsATT(Z, newZ, radem, trueTau)
      
      cbHats[j] <- tHat + th[2] + th[5]
      
    }
    
    # After bootstrapping, get variances of bHats and cbHats
    
    vHats[i] <- var(bHats)
    cvHats[i] <- var(cbHats)
    
    cat("Iteration = ", i, "\n", file=stdout())
  }
  
  # After all iterations are run, return variances
  
  out <- cbind(vHats, cvHats, targets)
  
  return(out)
  
}
