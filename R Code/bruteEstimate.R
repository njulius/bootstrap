bruteEstimate <- function(newZ) {
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # Correct estimate (maybe)
  # calulate K^2
  k2 <- newZ[,5]^2
  intermediate <- newZ[,4] * k2
  correct <- (1/n1)*sum(intermediate)
  
  # Wrong estimate (maybe)
  intermediateTwo <- newZ[,4] * newZ[,5]
  incorrect <- (1/n1)*sum(intermediateTwo)
  
  return(rbind(correct, incorrect))
}
