appendyHat <- function(Z, matches) {
  
  # Split sample
  
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # Make output container
  
  tauHat <- getATET(Z, matches)
  
  # Generate column of counterfactuals
  
  yHat <- rep(NA, times = n0 + n1)
  
  for(i in 1:n1) {
    mates <- controlSample[which(matches[,i] == TRUE),]
    
    # Dumb hack
    if(NCOL(mates) == 1) {
      yHat[i] <- mean(mates[3])
    } else {
      yHat[i] <- mean(mates[,3])
    }
    
  }
  
  Z <- cbind(Z, yHat)
  
  # Generate rademacher draws
  
  radem <- sample(c(-1,1), replace = TRUE, size = n0 + n1)
  
  Z <- cbind(Z, radem)
  
  return(Z)
  
}
