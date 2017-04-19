trueVar <- function(n, ratio) {
  
  n1 <- round((ratio / (1+ratio)) * n)
  n0 <- n - n1
  
  interVarOne <- 1/n1
  interVarTwo <- (n1 - 1) * (n0 + (8/3))
  interVarTre <- n1 * (n0 + 1) * (n0 + 2)
  
  tVar <- interVarOne + (3/2) * (interVarTwo / interVarTre)
  
  return(tVar)
  
}
