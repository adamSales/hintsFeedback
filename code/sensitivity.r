
###########################################################
#### sensitivity analyses
###########################################################
if(!exists("workMedMods")|!exists("corrMedMods")) load('results/regressions.RData')
attach(workMedMods)
attach(corrMedMods)


### for inclusion in main text: benchmarking by posttest
bm <- list(
bmworkPost = ovb_bounds(workPostReg,treatment="perWorkedC",benchmark_covariates="pretestC",kd=1:12),
bmworkState = ovb_bounds(workStateReg,treatment="perWorkedC",benchmark_covariates="pretestC",kd=1:12),
bmcorrPost = ovb_bounds(corrPostReg,treatment="perCorrC",benchmark_covariates="pretestC",kd=1:12),
bmcorrState = ovb_bounds(corrStateReg,treatment="perCorrC",benchmark_covariates="pretestC",kd=1:12)
)

pretestBM <- sapply(bm,function(x) min(which(x$adjusted_lower_CI<0) ))

sensitivity <- list(
  sensworkPost = sensemakr(workPostReg,treatment="perWorkedC",benchmark_covariates="pretestC",kd=1:12),
  sensworkState = sensemakr(workStateReg,treatment="perWorkedC",benchmark_covariates="pretestC",kd=1:12),
  senscorrPost = sensemakr(corrPostReg,treatment="perCorrC",benchmark_covariates="pretestC",kd=1:12),
  senscorrState = sensemakr(corrStateReg,treatment="perCorrC",benchmark_covariates="pretestC",kd=1:12))

save(pretestBM,sensitivity,file='results/sensitivity.RData')
