### Ripping off Mike R. and trying to test statistical coverage for Chao richness

s_pool=60 # Pool 
n_ind=1e3 #number of individuals to simulate
sad_coef=list(cv_abund=5)

minind<-1e2
maxind<-3.2e2
indSteps <- 3 #this determines how many sample sizes between minind and maxind to take

bootSamps<-4e2 # Ideally should be higher
reps<-5e1

#############
#load functions and packages
library(tidyverse)
library(EntropyEstimation)
library(furrr)#parallelization
library(scales)#trans_breaks
library(mobsim)#simulate communities
library(cowplot) #sometimes nice stuff for multipanel ggplotting

select<-dplyr::select

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
	data.bt = rmultinom(bootSamps,sampleSize,Bt_prob_abu(sam))
	obs<-dfun(sam,l)
	pro = apply(data.bt,2,function(boot)Chao_Hill_abu(boot,q)) 
	pro<-pro-mean(pro)+obs
	chaotile<-sum(pro<=truemun)/(bootSamps/100)
	return(data.frame(
		"chaotile"=chaotile, "truemu"=truemun
		,	"obsD"=obs, "l"=l, "sampleSize"=sampleSize
	)) 
}

####################
#run this whole thing to get sample diversity checkplot-type info for sample diversity for a single community
indSeq <- round(exp(seq(log(minind), log(maxind), length.out=indSteps)))
trycheckingobs_R<-map_dfr(indSeq, function(sampleSize){
	truemun<-truemu(usersguide, size=sampleSize, reps=reps, l=1)
	future_map_dfr(1:reps, function(reps){
	 	obscp(l=1, sampleSize, bootSamps, usersguide, truemun)
	})
})

write.csv(trycheckingobs_R, file="csvname", row.names=F)

quit()

##################################
# read in data and make checkplot for sample diveristy
trycheckingobs<-read.csv("data/fromR/trycheckingobs_R.csv")

#####################
# #checkplot figure, not used in users guide
# map(c(-1,0,1), function(ell){
#		trycheckingobs %>% filter(l==ell) %>% 
#			ggplot(aes(chaotile))+
#			geom_histogram()+
#			theme_classic()+
#			facet_wrap(~size+l)
# })

trycheckingobs_R %>% 
	ggplot(aes(chaotile))+
	geom_histogram()+
	theme_classic()+
	labs(x=paste("percentile of mean sample richness in", actual_boot, "bootstrap samples"), y="frequency")+
	facet_wrap(~size)
