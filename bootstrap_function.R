# Abadie & Imbens (2008) Simulation
# Author: Nik Julius
# Last Modified: July 28, 2016

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
  
  matchingIndex <- matrix(, nrow = n0, ncol = n1)
  
  # In matchingIndex, the column corresponds to the treated observation we seek to match. Each number
  # in that column is an index for the closest matching control observation. In theory, there could be
  # as many as n0 matching control observations.
  
  # Step 1: Find a closest match
  
  for(i in 1:n1) {
    
    # Assume match is index 1, then replace matching index each time we find a closer match
    
    x <- treatedBootstrap[i,1]
    
    matchingIndex[1,i] <- 1
    
    for(j in 1:n0) {
      
      dist <- abs(x - controlBootstrap[j,1])
      
      if(dist < abs(x - controlBootstrap[matchingIndex[1,i],1])) {
        matchingIndex[1,i] <- j
      } else {
        matchingIndex[1,i] <- matchingIndex[1,i]
      }
    }
    
    # Now, for treatedBootstrap[i,1], matchingIndex[1,i] contains the closest match. We proceed
    # to check for multiple close matches, and fill in matchingIndex[q,i], increasing q each time
    # we find another match
    
    numMatches <- 0 # This prevents the same match from being recorded twice
    # Frankly this step probably does everything but I'll optimize later
    
      for(j in 1:n0) {
        dist <- abs(x - controlBootstrap[j,1])
      
        if(dist == abs(x - controlBootstrap[matchingIndex[1,i],1])) {
          numMatches <- numMatches + 1
          matchingIndex[numMatches,i] <- j
        } else {
          matchingIndex[1,i] <- matchingIndex[1,i]
        }
      }
    
    }
  
  # Now we want to construct estimates of the untreated outcome for our treated units. We do this
  # by taking the outcome for their closest match if there is only 1 match, and by taking the average
  # of outcomes if there is more than one match
  
  y0Hat <- rep(0, times = n1)
  
  for(i in 1:length(treatedBootstrap[,1])) {
    
    # Check if there is more than one match
    if(is.na(matchingIndex[2,i])) {
      # Then only one match
      y0Hat[i] <- controlBootstrap[matchingIndex[1,i],3]
    } else {
      # There is more than one match, thus find number of matches
      count <- n0 - sum(is.na(matchingIndex[,i]))
      # Sum y0's for the matches
      intermediateSum <- 0
      for(j in 1:count) {
        intermediateSum <- intermediateSum + controlBootstrap[matchingIndex[j,i]]
      }
      y0Hat[i] <- intermediateSum / count
    }
  }
  
  # Now, we can construct a vector of treatement effect estimates, one from each observation in
  # the treated bootstrap sample
  
  tHats <- treatedBootstrap[,3] - y0Hat
  tHat <- mean(tHats)
  
  return(tHat)
}
