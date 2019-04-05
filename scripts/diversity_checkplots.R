# script to make checkplots for Chao's variance estimators for diversity estimates. Will start with Simpsons
library(tidyverse)
library(furrr)
source("scripts/helper_funs/estimation_funs.R")
library(mobsim)

# #start with JD comms
# mini<-100
# com1 <- c(1, 1, 1, 1)
# com2 <- c(1, rep(1/(mini-1), mini))
# #add a more naturalistic one
# com3<-as.numeric(sim_sad(s_pool=60, n_sim=100000))
# #this is supersampling
# o1<-sample(1:length(com1), prob=com1, size=inds, replace=T)
# o2<-sample(1:length(com2), prob=com2, size=inds, replace=T)

#three communities with richness ~60, and different skew
com1<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=2)))
com2<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=5)))
com3<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=3)))

plot(1:length(com1), com1)
plot(1:length(com2), com2)
plot(1:length(com3), com3)


asab<-function(namevec){as.numeric(table(namevec))}

dfun(com1, l=-1)
dfun(com2, l=-1)
dfun(com3, l=-1)

#gets slightly closer than obs

nc<-60#per Rob's recommendation
plan(strategy=multiprocess, workers=nc)

# make this cleaner
checkplot<-function(abs, B=2000, l, inds, reps){
    td<-dfun(abs, l)
    future_map_dfr(1:reps,function(x){
    obs<-subsam(abs, size=inds)
    chaotile<-checkchao(obs, B, l, td)
   return(chaotile=data.frame(chaotile, l=rep(l, reps), inds=rep(inds, reps), reps=rep(reps, reps)))})
   }
outerreps<-84
reps<-60
start<-Sys.time()
map(1:outerreps, function(x){
  tall<-map_dfr(c(150,300,750), function(inds){
      map_dfr(c("com1", "com2", "com3"), function(abs){
          map_dfr(c(-1,0,0.5,1), function(l){
              out<-checkplot(abs=get(abs), l=l, inds=inds, reps=reps)
              return(cbind(out, comm=rep(abs, length(out[,1]))))
          })
      })
  })
  write.csv(tall, paste("data/fromR/sims",x, ".csv"), row.names=F)
})
print(Sys.time()-start)



# t100<-do1000(com1, B=2000, l=1, inds=150, reps=100)

# 
# try1000timesSHan<-map_dfr(1:1000, function(x){
#     o3<-subsam(com3, size=inds)
#     return(do1000(o3, 1000, 0, dfun(com3, 0)))
# })

# try200times.5<-map_dfr(1:200, function(x){
#     o3<-subsam(com3, size=inds)
#     return(do1000(o3, 400, .5, dfun(com3, .5)))
# })
# 
# sum(try200times.5$chaotile<2.5)/1000000+sum(try200times.5$chaotile>97.5)/1000000
# 
# 
# 
# sum(try1000timesSHan$chaotile<2.5)/1000000+sum(try1000timesSHan$chaotile>97.5)/1000000
# 
# pdf(file="figures/repeated_checkplot_with_same_comm_point5.pdf")
# hist(try200times.5$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="200 random draws from naturalistic community \n l=0.5")
# dev.off()
# 
# hist(try200times.5$chaotile[1601:2000], xlab="true value as percentile of Chao boot", xlim=c(0,100), main="200 random draws from naturalistic community \n l=0.5")
# 
# pdf(file="figures/repeated_checkplot_with_same_comm_SH.pdf")
# hist(try100timesSHan$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="1000 random draws from naturalistic community \n rich")
# dev.off()
# 
# firstout<-do1000(o3, 1000, 0, dfun(com3,0))
# pdf(file="figures/first_naturalistic_Shannon_checkplot.pdf")
# hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main=paste("naturalistic lognormal community with Shannon-Hill=", round(truediv,2)))
# dev.off()
# 
# firstout<-do1000(asab(o1), 1000, -1, dfun(com1,-1))
# pdf(file="figures/first_even_Simpson_checkplot.pdf")
# hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="even community with Simpson-Hill=4")
# dev.off()
# 
# firstout<-do1000(asab(o2), 1000, -1, dfun(com2,-1))
# pdf(file="figures/first_Simpson_checkplots.pdf")
# hist(firstout$chaotile, xlab="true value as percentile of Chao boot", xlim=c(0,100), main="skewed community with Simpson-Hill=4")
# dev.off()
# 
