#in this step

# 6)   Grab samples of say 5e1, 1e2, 5e2, 1e3, 5e3, and 
#for each one compute Chao's estimated diversities (all 3) 
# for a bunch of sample coverages (5 values? 10 values?) where we have narrow bins. Record for each
# 
# editing this plan to grab samples of c(100, 200, 500, 1000, 2000, 5000, 1000), 
# will need to do coverage rarefaciton in next step
# 

#load communities from before 
load("my_coms.RData")

library(furrr)
source("scripts/helper_funs/uniroot_gamma_and_lnorm.R")
R_FUTURE_FORK_ENABLE=T
library(iNEXT)
library(tictoc)
library(data.table)
#

bs<-fread("data/comm_samp.csv")


SS<-c(100, 200, 500, 1000, 2000, 5000, 1000)
reps<-4

nc<-4
plan(strategy = multiprocess, workers = nc)


tic()
compare_samples<-future_map_dfr(1:reps
                                 # , .options = future_options(globals(structure=T, add=c("reps", "SS", "gamma_comm", "lnorm_comm", "sample_infinite"
                                 # , "dfun", "compute_cov"))
                                 , function(rep){
                                     map_dfr(SS, function(indis){
                                         map_dfr(1:length(clist), function(SAD){
                                             mydst=clist[[SAD]][[3]]
                                             myabs=sample_infinite(mydst, indis)
                                             # 3) For each sample, compute true coverage, Hill diversity with ell={-1,0,1}
                                             # 
                                             rich=sum(myabs>0)
                                             shan=dfun(myabs,0)
                                             simp=dfun(myabs,-1)
                                             cov=sum((myabs>0)*mydst)
                                             ecov<-iNEXT:::Chat.Ind(myabs, sum(myabs>0))
                                             return(data.frame(t(myabs), SS=indis, comm=names(clist)[SAD]
                                                               , rich=rich, shan=shan, simp=simp, tc=cov, ec=ecov))
                                         })
                                     })
                                 })
toc()
