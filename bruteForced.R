# A function that bruteforces a correct bootstrap estimator
# using knowledge about the DGP

bruteForced <- function(Z, matches) {
  # Get e_i for every unit in Z
  n <- length(Z[,1])
  errors <- rep(0, times = n)
  
  # Get K_i
  K <- rowSums(matches)
  
  # Split sample because I'm too stupid to do this right
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # Stack sample so control units come first
  
  newZ <- rbind(controlSample, treatedSample)
  
  # Go through and set e_i to correct value
  for(i in 1:n) {
    if(newZ[i,2] == 0) {
      errors[i] <- newZ[i,3] - 0
    } else {
      errors[i] <- 0
    }
  }
  
  # Add errors as a column to newZ
  nnZ <- cbind(newZ, errors)
  
  # Append n1 zeros to K so that it becomes a vector of length 0. It will be correctly filled in
  # because I split and stacked the sample in the right way.
  
  nK <- c(K, rep(0,times=n1))
  
  # Add K as a column to nnZ
  
  nnnZ <- cbind(nnZ, nK)
  
  # IT WORKS
  
  return(nnnZ)
}

