# R Functions

1. genData()

  Function takes arguments characterizing the number of observations to generate, the ratio of treated observations to untreated observations, and the true treatment effect, in that order. Function generates appropriate treated and untreated observations according to assumptions detailed in Abadie & Imbens (2008) (Assumptions A3.1 through A3.4).
2. naiveBootstrap()
  
  Function takes one argument, which is expected to be the output of genData(). In general, the argument should be a N x 3 matrix, with the columns corresponding to covariate, treatment indicator, and outcome respectively. Function performs a single naive bootstrap, sampling treated and untreated observations with replacement until it has a bootstrapped set of observations with an identical treated-to-untreated ratio as the input set, then returns an estimate of the treatment effect from the bootstrapped dataset.
