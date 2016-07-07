# Abadie & Imbens (2008) Simulation
# Author: Nik Julius
# Last Modified: June 29, 2016

# This script requires that genData() and naiveBootstrap() are present in the R environment.

numSims <- 50

# numSims tells us how many different datasets to generate in estimating the true distribution of
# the matching estimator

# I THINK I NEED TO WRITE A FUNCTION THAT JUST GETS A TAUHAT ESTIMATE FROM A DATASET TO MAKE THIS EASY

tHatTrue <- rep(0, times = numSims)

for(i in 1:length(tHatTrue)) {
  
}