#check statistical coverage for CI for coverage-standardized samples

library(furrr)
source("scripts/helper_funs/uniroot_gamma_and_lnorm.R")
R_FUTURE_FORK_ENABLE=T
library(iNEXT)
# simulate full community
fullcomm<-fit_SAD(rich=100, simpson=20)

#get each sample size 5k times
nc<-7
plan(strategy = multiprocess, workers = nc)

reps<-500
SS<-200
somedata_to_think_about<-future_map_dfr(1:reps, function(rep){
    # map_dfr(floor(10^seq(1.5,4,0.1)), function(SS){
        data.frame(SS=SS, estimateD(sample_infinite(fullcomm[[3]], SS), base="coverage", level=.75))
    })


