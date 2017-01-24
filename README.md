# R Functions (Outdated)

1. genData()

  Function takes arguments characterizing the number of observations to generate, the ratio of treated observations to untreated observations, and the true treatment effect, in that order. Function generates appropriate treated and untreated observations according to assumptions detailed in Abadie & Imbens (2008) (Assumptions A3.1 through A3.4).
2. naiveBootstrap()
  
  Function takes one argument, which is expected to be the output of genData(). In general, the argument should be a N x 3 matrix, with the columns corresponding to covariate, treatment indicator, and outcome respectively. Note that if the indicator column contains anything other than 0 or 1, things are probably going to go wrong. Function performs a single naive bootstrap, sampling treated and untreated observations with replacement until it has a bootstrapped set of observations with an identical treated-to-untreated ratio as the input set, then returns an estimate of the treatment effect from the bootstrapped dataset.
3. aiSimulation()

  Function takes 3 arguments that are to be fed to genData(), and one scalar argument that determines how many bootstraps to run. Function returns the value of N1 * { E[(tauHat_b - tauHat)^2 | Z] - Var(tauHat) }, which should converge in probability to 0 according to Abadie & Imbens (2008).
4. findMatches()
  
  Function takes one argument, which is expected to be the output of genData(). See naiveBootstrap() for more on this expectation. Function returns a sparse logical matrix, which will be explained better here when I feel up to figuring out how to get a matrix onto this.
