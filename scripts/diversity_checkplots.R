# script to make checkplots for Chao's variance estimators for diversity estimates. Will start with Simpsons
library(tidyverse)
library(furrr)
source("scripts/helper_funs/estimation_funs.R")
library(mobsim)

#start with JD comms
mini<-100
com1 <- c(1, 1, 1, 1)
com2 <- c(1, rep(1/(mini-1), mini))
#add a more naturalistic one
com3<-as.numeric(sim_sad(s_pool=60, n_sim=100000))
#going to take samples of size inds from each

inds<-350
 #this is supersampling
o1<-sample(1:length(com1), prob=com1, size=inds, replace=T)
o2<-sample(1:length(com2), prob=com2, size=inds, replace=T)
#this is subsampling
o3<-subsam(com3, size=inds)

asab<-function(namevec){as.numeric(table(namevec))}

dfun(asab(o1), l=-1)
dfun(asab(o2), l=-1)
dfun(com1, l=-1)
dfun(com2, l=-1)
dfun(com3, l=-1)
dfun(o3, l=-1)

#gets slightly closer than obs
Chao_Hill_abu(asab(o1), q=2)
#also slightly closer than obs but still far off and downward biased?
Chao_Hill_abu(asab(o2), q=2)

Chao_Hill_abu(o3, q=2)
# checkchao(asab(o1), B=1000, l=-1, truediv=4)

nc<-parallel::detectCores()-1
plan(strategy=multiprocess, workers=nc)

abs<-o3
B<-1000
l<--1
truediv<-dfun(com3, l=-1)

do1000<-function(abs, B, l, truediv){future_map_dfr(1:1000,function(x){
    chaotile<-checkchao(abs, B, l, truediv)
    return(chaotile=data.frame(chaotile))})
    }




try1000times<-map_dfr(1:1000, function(x){
    o3<-subsam(com3, size=inds)
    return(do1000(o3, 1000, -1, dfun(com3, -1)))
})

firstout<-do1000(o3, 1000, -1, dfun(com3,-1))
pdf(file="figures/first_naturalistic_Simpson_checkplot.pdf")
hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main=paste("naturalistic lognormal community with Simpson-Hill=", round(truediv,2)))
dev.off()

firstout<-do1000(asab(o1), 1000, -1, dfun(com1,-1))
pdf(file="figures/first_even_Simpson_checkplot.pdf")
hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="even community with Simpson-Hill=4")
dev.off()

firstout<-do1000(asab(o2), 1000, -1, dfun(com2,-1))
pdf(file="figures/first_Simpson_checkplots.pdf")
hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="skewed community with Simpson-Hill=4")
dev.off()

