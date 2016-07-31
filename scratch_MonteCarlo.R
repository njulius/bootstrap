# ai Path Example
# scratch work

obsSeq = seq(from = 100, to = 1000, by = 50)
boots = 300

treatRatio = 1

diffs <- rep(0, times=length(obsSeq))

for(i in 1:length(obsSeq)) {
  diffs[i] <- aiSimulation(obsSeq[i], treatRatio, 5, boots)
}
