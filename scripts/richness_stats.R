### Ripping off Mike R. and trying to test statistical coverage for Chao richness

lmin <- 1
lmax <- 1
s_pool=60 # Pool 
n_ind=1e3 #number of individuals to simulate
sad_coef=list(cv_abund=5)

minind<-1e2
maxind<-3.2e2
indSteps <- 3 #this determines how many sample sizes between minind and maxind to take

bootSamps<-2e2 
checkSamps<-2e3
muReps<-2e3

#############
#load functions and packages
library(tidyverse)
library(EntropyEstimation)
library(furrr)#parallelization
library(scales)#trans_breaks
library(mobsim)#simulate communities
library(cowplot) #sometimes nice stuff for multipanel ggplotting

#make a community for User's Guide
#sim_sad 
usersguide<-as.numeric(sim_sad(s_pool=s_pool 
	, n_sim=n_ind #number of individuals to simulate
	, sad_coef=sad_coef
))

#.see how many cores are on the system and use one fewer. 
# Good for a personal computer. 
nc<-parallel::detectCores()-1
plan(strategy=multiprocess, workers=nc) 

#Generate quantiles of bootstrap distribution given true diversity, sample size, l, and true average
obscp<-function(l, sampleSize, dat, bootSamps, truemun){
	q <- 1-l
	sam<-subsam(dat, sampleSize)
	aProb <- Bt_prob_abu(sam)
	data.bt = rmultinom(bootSamps,sampleSize,aProb)
	obs<-dfun(sam,l)
	pro = apply(data.bt,2,function(boot)Chao_Hill_abu(boot,q)) 
	pro<-pro-mean(pro)+obs
	chaotile<-sum(pro<=truemun)/(bootSamps/100)
	return(data.frame(
		"chaotile"=chaotile, "truemu"=truemun
		,	"obsD"=obs, "l"=l, "sampleSize"=sampleSize
	)) 
}

######################################################################

## For stepping; doesn't hurt much
## Can be deleted when we understand things better
l <- 1
dat <- usersguide
sampleSize <- 200

truemun<-truemu(usersguide, size=sampleSize, reps=muReps, l=l)

q <- 1-l
sam<-subsam(dat, sampleSize)
obs<-dfun(sam,l)
pro = replicate(bootSamps, {
	aProb <- Bt_prob_abu_samp(sam)
	aProb[is.na(aProb)] <- 0
	data.bt = rmultinom(1,sampleSize,aProb)
	return(Chao_Hill_abu(data.bt,q))
})
pro<-pro-mean(pro)+obs
chaotile<-sum(pro<=truemun)/(bootSamps/100)


######################################################################
## Main loop

for(l in lmin:lmax){
	indSeq <- round(exp(seq(log(minind), log(maxind), length.out=indSteps)))
	checkFrame <-map_dfr(indSeq, function(sampleSize){
		truemun<-truemu(usersguide, size=sampleSize, reps=muReps, l=l)
		future_map_dfr(1:checkSamps, function(r){
			obscp(l=l, sampleSize, usersguide, bootSamps, truemun)
		})
	})

	print(ggplot(checkFrame, aes(chaotile))
		+ geom_histogram()
		+ theme_classic()
		+ labs(
			x=paste("percentile of sample diversity in"
				, bootSamps, "bootstrap samples"
			)
			, y="frequency"
		)
		+ facet_wrap(~sampleSize)
	)
}
