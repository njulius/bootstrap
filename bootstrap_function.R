# Abadie & Imbens (2008) Simulation
# Author: Nik Julius
# Last Modified: June 29, 2016

# A function that performs a single naive bootstrap of a dataset Z=(X,W,Y) and returns
# the bootstrap estimator of the treatment effect

naiveBootstrap <- function(Z) {
  # Variables 
  #
  # Z should be the output of the function genData(), or in general a matrix with 3
  # columns corresponding to X, W, and Y
  
  # First, we need to separate Z into the two subsamples consisting of treated units
  # (W[i] = 1) and control units (W[i] = 0)
  
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # Construct bootstrap sample
  
  treatedBootstrap <- treatedSample[sample(nrow(treatedSample), size = n1, replace = TRUE),]
  controlBootstrap <- controlSample[sample(nrow(controlSample), size = n0, replace = TRUE),]
  
  # Construct matching estimator
  
  # Find matches for each X[i] in the treated sample
  
  matchingIndex = rep(0, times = n1)
  
  for(i in 1:length(treatedBootstrap[,1])) {
    x <- treatedBootstrap[i,1]
    
    # Assume match is index 1, then replace matching index each time
    # we find a closer match. There is certainly a better way of doing
    # this
    
    # This finds one of the indices for which the match is closest. It
    # DOES NOT find more than one, and will return the lowest index only
    # if more than one match of the same closeness is present in the bootstrap
    # sample
    matchingIndex[i] <- 1
    
    for(j in 1:length(controlBootstrap[,1])) {
      dist <- abs(x - controlBootstrap[j,1])
      
      if(dist < abs(x - controlBootstrap[matchingIndex[i],1])) {
        matchingIndex[i] <- j
      } else {
        matchingIndex[i] <- matchingIndex[i]
      }
    }
    
  }
  
  # The variable matchingIndex now contains n1 numbers that give us one of the
  # closest matches to each treated unit. For now, we operate under the assumption
  # that if multiple matches are present, this is ONLY because the same control
  # unit was sampled multiple times in the bootstrap - that is, if there are multiple
  # matches, all of the matches are the same original observation. We will sanity 
  # check this later on
  
  numMatches <- rep(0, times = n1)
  
  for(i in 1:length(treatedBootstrap[,1])) {
    x <- treatedBootstrap[i,1]
    
    # We proceed essentially the same way as before, except when we find a new match
    # we increment numMatches[i] instead of recording the match index
    
    for(j in 1:length(controlBootstrap[,1])) {
      dist <- abs(x - controlBootstrap[j,1])
      
      if(dist == abs(x - controlBootstrap[matchingIndex[i],1])) {
        numMatches[i] <- numMatches[i] + 1
      } else {
        numMatches[i] <- numMatches[i]
      }
    }
  }
  
  # Now numMatches[i] tells us how many control unit matches there are for each treated
  # unit i in the bootstrap sample.
  
  ####
  #
  # I have not figured out how to sanity check the previous assumption, so we're proceeding
  # as if it doesn't matter. I'm pretty sure it actually doesn't, to be honest.
  #
  ####
  
  # Now we want to construct estimates of the untreated outcome for our treated units. We do this
  # by taking the outcome for their closest match.
  
  y0Hat <- rep(0, times = n1)
  
  for(i in 1:length(treatedBootstrap[,1])) {
    y0Hat[i] <- controlBootstrap[matchingIndex[i],3]
  }
  
  # Now, we can construct a vector of treatement effect estimates, one from each observation in
  # the treated bootstrap sample
  
  tHats <- treatedBootstrap[,3] - y0Hat
  tHat <- mean(tHats)
  
  return(tHat)
}