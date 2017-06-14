# Optimized function to get the ATET

getATET <- function(Z, matches) {
  # Split sample
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  # Get N1 and N0
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # First, I want to generate counterfactual outcomes
  # for each treated unit.
  
  # To do this, I average the outcomes of all the matched
  # control units. To do THIS, I create an intermediate
  # matrix for convenience.
  
  inter <- matrix(controlSample[,3], nrow=length(controlSample[,3]), ncol=n1)
  
  # This matrix has N1 identical columns. Each column contains
  # the outcome (Y) for the i'th control unit in the i'th row.
  # Thus, if I can multiply element-wise this matrix with
  # the matching matrix, I will end up with a matrix where all
  # I need are the averages of ALL NONZERO elements in each
  # column. This average in the j'th column will be the counterfactual for
  # the j'th treated unit.
  
  # The number of matches to each treatment unit are
  
  nMatches <- colSums(matches)
  
  # The summed counterfactual outcomes to each treatment unit are
  
  summedCF <- colSums(inter * matches)
  
  # Thus the counterfactuals for the n1 treated units are
  
  counterFactuals <- summedCF / nMatches
  
  # This should be of length n1
  
  # Thus the treatment effect estimator for each treated unit is
  
  unitTET <- treatedSample[,3] - counterFactuals
  
  # And, finally, the ATET is the mean of unitTET
  
  ATET <- mean(unitTET)
  
  return(ATET)
}
