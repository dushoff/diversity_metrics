##### code, finally, to compare asymptotic and 3 ways of doing rarefaction. 

library(tidyverse)
library(mobsim) #for simulating SADs with sim_sad
library(furrr) #does parallelization
library(iNEXT) # Chao's package for doing coverage etc.
library(scales)
source("scripts/helper_funs/estimation_funs.R")
source("scripts/helper_funs/read_tcsv.R")


#load microbial data used in Haegeman 2013

haegdat <- read.tcsv("data/Haegeman_data.csv") 
# deal with NAs added in previous line
haegdat[is.na(haegdat)]<-0
# #remove first sample, much smaller
# haegdat<-haegdat[,-1]
# #repeat, last is bad too, relatively
# haegdat<-haegdat[,-8]
#seems like we get crazy stuff if we use wildly incomparable things. So stick with the four soil communities, see if it works at all. 

haegdat<-haegdat[,2:5]
#try doubling haegdat values to get 100% coverage by definition
haegdat<-haegdat*2

# #make communities. 3=1+2. 4=2+2. 1 more even. 
comm1<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=5)))
comm2<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=9)))
comm3<-c(comm1, comm2)
comm4<-(c(comm2, comm2))

mikedat<-data.frame(comm1=c(comm1, rep(0,120)), comm2=c(comm2, rep(0,120)), comm3=comm3, comm4=comm4)

# look at differences on log scale

#beware of using variables this way.. this loop was for comms 1-4
# out<-map_dfr(1:4, function(comm){
#     map_dfr(c(-1,0,1), function(l){
#         data.frame(logdiv=log(dfun(get(paste("comm", comm, sep="")), l=l)), comm=comm, l=l)
#     })
# })


#name dataset
mydat<-haegdat

out<- map_dfr(c(-1,0,1), function(l){
        map_dfr(names(mydat), function(comm){
    data.frame(logdiv=log(dfun(mydat[,comm], l=l)), comm=comm, l=l)
  })
})


#just see what total abundances are for selected comms
map(names(mydat), function(x){
  sum(mydat[,x])
})

########
# original for k, using simulated communities
# k<-map_dfr(c(-1,0,1), function(m){
#   dists<-as.numeric(dist(out %>% filter(l==m) %>% select(logdiv), method="manhattan"))
#   names(dists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour" )
#   dists
#   return(data.frame(t(dists), m=m))
#   
# })

#this was modified for mydat
k<-map_dfr(c(-1,0,1), function(m){
  dists<-as.numeric(dist(out %>% filter(l==m) %>% select(logdiv), method="manhattan"))
  names(dists)<-unite(data.frame(t(combn(names(mydat),2))))[,1]
  return(data.frame(t(dists), m=m))

})

k<-k %>% gather(diff_btwn, val, 1:choose(length(mydat),2)) %>% select(diff_btwn, trueval=val, l=m)

################################################################################################
###### This is a giant routine that will take a long time and tons of compute resources ########
################################################################################################

#set number of cores manually. This 64 is for running on Annotate after checking that no other big users with top
nc<-36
plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands
# one rep takes a long time on one fast core. I think estimateD might be the slow function. 
nreps<-72



rarefs_mikedat<-future_map_dfr(1:nreps, function(reps){
  map_dfr(floor(10^seq(2,3.1,.05)), function(inds){ #sample sizes
    mySeed<-1000*runif(1)
    set.seed(mySeed)
    # set.seed(131.92345)
    # inds<-223
    rare<-lapply(names(mydat), function(com){subsam(mydat[,com], inds)}) #rarefy each community
    names(rare)<-names(mydat)
    covdivs<-tryCatch(estimateD(rare, base="coverage"), error=function(e) data.frame(site=rep(names(mydat),3), order=rep(c(-1,0,1), length(mydat)), qD=rep(mySeed, 3*length(mydat)), SC=rep(mySeed, 3*length(mydat)))) #use iNEXT::estimateD to compute expected Hill diversities for equal coverage
    map_dfr(names(mydat), function(com){ #then loop over communities again, b/c going to return one row for each combination of sample size, community, hill exponent
      map_dfr(c(-1,0,1), function(ell){ #hill exponents
        samp<-dfun(rare[[com]], l=ell) #compute sample diversity
        chaoest<-tryCatch(Chao_Hill_abu(rare[[com]], q=1-ell), error=function(e) "whoops") #compute asymptotic estimator; Chao uses q=1-l
        return(tryCatch(data.frame(samp=samp, chaoest=chaoest
                                   , cover=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "qD"] #this is diversity
                                   , coverage=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "SC"] #this is coverage estimate
<<<<<<< HEAD
                                   , cov_size=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "m"] #this is coverage estimate
                                   , l=ell, size=inds, comm=com, reps=reps) #not sure this error thing is necessary but seems conservative to keep it
=======
                                   , cov_size=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "m"] #this is thes sample size implied by coverage estimate
                                   , l=m, size=inds, comm=com, reps=reps) #not sure this error thing is necessary but seems conservative to keep it
>>>>>>> f392e3b5fbd3b9c2499e315de73840e5fc85e5a5
                        , error=function(e) data.frame(samp=samp, chaoest=chaoest, cover=NA, coverage=NA, cov_size=NA, l=ell, size=inds, comm=com, reps=reps)))
      })
    })
  })
})




#write data to file

write.csv(rarefs_mikedat, file="data/coverage_vs_others_haegdat_with_cov_size.csv", row.names=F)

# rarefs<-read_csv("data/coverage_vs_others_haeg1.csv")

rarefsl<-rarefs_mikedat %>% mutate(chaoest=log(chaoest), samp=log(samp), cover=log(cover))


#################
# summarize differences
rarediffs<-rarefsl %>% 
  gather(meth, esti, samp, chaoest, cover) %>%   
  group_by(l, size, reps,  meth) %>% 
  do(diffbetween=data.frame(t(combn(.$comm, m = 2))) %>% 
       unite(sitio) %>% pull(sitio)
     , diffs=combn(.$esti, m=2, diff)
     ) %>% 
    unnest()




# stupid renaming, should clean up
k<-k %>% mutate(diffbetween=diff_btwn)

## set so it's always big-little, a positive number, this is what the dist thing does and it allows order not to matter, which is good. switch from positive to negative not expected between runs, but imaginable for richness at low coverage, or I guess anything, but then should be small differences and consistent with a low RMSE either way.
rarediffs$diffs<-abs(rarediffs$diffs)

##compute RMSE against true differences in diversity between comms
rmses<-map_dfr(floor(10^seq(2,3.1,.05)), function(inds){
  sqe<-rarediffs %>% filter(size==inds) %>% left_join(k) %>% mutate(sqdiff=(trueval-diffs)^2, method=meth)

  evalu<-sqe %>% group_by(l, method) %>% summarize(rmse=sqrt(mean(sqdiff, na.rm=TRUE)))
  return(data.frame(evalu, size=inds))
})

#just rename for human legibility
rmses<-rmses %>% left_join(data.frame(l=c(-1,0,1), hill=c("Hill-Simpson", "Hill-Shannon", "Richness")))
# set factor levels for plot ordering
rmses$hill<-factor(rmses$hill, levels=c("Hill-Simpson", "Hill-Shannon", "Richness"))


## Show RMSE as a function of sample size for each hill number, each method with different color/point.
pdf(file="figures/sample_a_lot_use_coverage.pdf")
rmses %>% ggplot(aes(size, rmse, color=method, shape=method))+
  geom_point(alpha=0.5, size=.75)+
  geom_line(alpha=0.5, size=0.5)+
  facet_wrap(~hill)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  labs(x="sample size (individuals)", y="RMSE in predicting pairwise differences \nin log(diversity) between communities")
dev.off()


#############
# find intersection
findint<-rmses %>% spread(method, rmse) %>%  group_by(hill, size) %>% mutate(chaowins=(samp-chaoest)>0) %>% filter(hill=="Richness")
View(findint)
#crossing around 446

rarefsl %>% filter(size==251) %>% ggplot(aes(coverage))+geom_histogram()+facet_wrap(~comm)

######### BASED ON THIS I HAVE NO IDEA WHAT'S GOING ON. MAYBE IT'S 90% COVERAGE THOUGH. 

##################
# rough plot to visualize
rarefs[which(rarefs$comm==1), "comm"]<-"a"
rarefs[which(rarefs$comm==2), "comm"]<-"b"
rarefs[which(rarefs$comm==3), "comm"]<-"a+b"
rarefs[which(rarefs$comm==4), "comm"]<-"b+b"

pdf(file="figures/rough_rae_fig.pdf")
rarefs %>% 
    gather(etype, est, chaoest, samp, cover) %>% 
    ggplot(aes(x=size, y=est, color=comm, fill=comm))+
        geom_smooth()+
        scale_y_log10()+
        scale_x_log10()+
        facet_wrap(~etype+l, scales="free")+
        labs(y="estimated diversity")+
        theme_classic()
dev.off()

