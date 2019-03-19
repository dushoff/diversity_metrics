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
com3<-sim_sad(s_pool=60, n_sim=100000)
#going to take samples of size inds from each

inds<-350

o1<-sample(1:length(com1), prob=com1, size=inds, replace=T)
o2<-sample(1:length(com2), prob=com2, size=inds, replace=T)
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
checkchao(asab(o1), B=1000, l=-1, truediv=4)

nc<-parallel::detectCores()-1
plan(strategy=multiprocess, workers=nc)

abs<-o3
B<-1000
l<--1
truediv<-dfun(com3, l=-1)
firstout<-future_map_dfr(1:1000,function(x){chaotile<-checkchao(abs, B, l, truediv)
    return(chaotile=data.frame(chaotile))})

pdf(file="figures/first_naturalistic_Simpson_checkplot.pdf")
hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="naturalistic lognormal community with Simpson-Hill=38.9")
dev.off()


pdf(file="figures/first_even_Simpson_checkplot.pdf")
hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="even community with Simpson-Hill=4")
dev.off()

pdf(file="figures/first_Simpson_checkplots.pdf")
hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="skewed community with Simpson-Hill=4")
dev.off()
