# wild bootstrap with recentering

# UNIT TESTED SUCCESSFULLY

wildBoot <- function(Z, matches) {
  
  # Split sample
  
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # Make output container
  
  tauHat <- getATET(Z, matches)
  
  interK <- rep(0, times = n1) # This is a disgusting hack
  K <- append(interK, rowSums(matches))
  counterFact <- rep(NA, times = n0 + n1)
  radembacher <- sample(c(-1,1), replace = TRUE, size = n0 + n1)
  
  # Return to normal operation
  
  resids <- rep(0, times = n1)
  
  for(i in 1:n1) {
    # Find matched control unit and construct estimated control outcome
    mates <- controlSample[which(matches[,i] == TRUE),]
    
    # This is a hack to get around a problem I don't even remember now
    if(length(mates) == 3) {
      counterFact[i] <- mean(mates[3])
    } else {
      counterFact[i] <- mean(mates[,3])
    }
    
    resids[i] <- treatedSample[i,3] - counterFact[i] - tauHat
    
  }
  
  # Perturb the treated outcomes
  
  bootTreated <- treatedSample
  
  for(i in 1:n1) {
    # Coinflips were done in drawing radembacher above
    bootTreated[i,3] <- counterFact[i] + tauHat + radembacher[i] * resids[i]
    
    
  }
  
  interXi <- rep(NA, times = n0)
  xiHat <- append(resids, interXi)
  
  # Finally, calculate idiosyncratic errors (on the original values of Y, NOT the bootstrapped ones (I think))
  
  treatedIdio <- rep(0, times = n1) # DGP guarantees this is true
  controlIdio <- controlSample[,3] # DGP guarantees this is true too
  
  idioError <- append(controlIdio, treatedIdio)
  
  stackedOut <- rbind(bootTreated, controlSample)
  
  out <- cbind(stackedOut, K, counterFact, xiHat, idioError, radembacher)
  
  return(out)
  
}
