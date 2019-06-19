##### code, finally, to compare asymptotic and 3 ways of doing rarefaction. 

library(tidyverse)
library(mobsim) #for simulating SADs with sim_sad
library(furrr) #does parallelization
library(iNEXT) # Chao's package for doing coverage etc.

source("scripts/helper_funs/estimation_funs.R")


#make communities. 3=1+2. 4=2+2. 1 more even. 
comm1<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=5)))
comm2<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=9)))
comm3<-c(comm1, comm2)
comm4<-(c(comm2, comm2))

# look at differences on log scale

#beware of using variables this way
out<-map_dfr(1:4, function(comm){
    map_dfr(c(-1,0,1), function(l){
        data.frame(logdiv=log(dfun(get(paste("comm", comm, sep="")), l=l)), comm=comm, l=l)
    })
})

k<-map_dfr(c(-1,0,1), function(m){
  dists<-as.numeric(dist(out %>% filter(l==m) %>% select(logdiv), method="manhattan"))
  names(dists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour" )
  dists
  return(data.frame(t(dists), m=m))
  
})

k<-k %>% gather(diff_btwn, val, 1:6) %>% select(diff_btwn, trueval=val, l=m)

################################################################################################
###### This is a giant routine that will take a long time and tons of compute resources ########
################################################################################################

#set number of cores manually. This 64 is for running on Annotate after checking that no other big users with top
nc<-64
plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands
# one rep takes a long time on one fast core. I think estimateD might be the slow function. 
nreps<-500


rarefs<-future_map_dfr(1:nreps, function(reps){
       map_dfr(floor(10^seq(2,5,.05)), function(inds){ #sample sizes
            rare<-lapply(1:4, function(com){subsam(get(paste("comm", com, sep="")), inds)}) #rarefy each community
            names(rare)<-1:4
            covdivs<-estimateD(rare, base="coverage") #use iNEXT::estimateD to compute expected Hill diversities for equal coverage
            map_dfr(1:4, function(com){ #then loop over communities again, b/c going to return one row for each combination of sample size, community, hill exponent
                map_dfr(c(-1,0,1), function(m){ #hill exponents
                    samp<-dfun(rare[[com]], l=m) #compute sample diversity
                    chaoest<-Chao_Hill_abu(rare[[com]], q=1-m) #compute asymptotic estimator; Chao uses q=1-l
                    return(tryCatch(data.frame(samp=samp, chaoest=chaoest
                                               , cover=covdivs[which(covdivs$site==com&covdivs$order==1-m), "qD"] #this is diversity
                                               , coverage=covdivs[which(covdivs$site==com&covdivs$order==1-m), "SC"] #this is coverage estimate
                                               , l=m, size=inds, comm=com, reps=reps) #not sure this error thing is necessary but seems conservative to keep it
                                    , error=function(e) data.frame(samp=samp, chaoest=chaoest, cover="err", coverage="err", l=m, size=inds, comm=com, reps=reps)))
            })
        })
    })
})



#write data to file

write.csv(rarefs, file="data/coverage_vs_others.csv", row.names=F)

rarefsl<-rarefs %>% mutate(chaoest=log(chaoest), samp=log(samp), cover=log(cover))

#this is clunky code but returns a df that compares pairwise differences in log diversity for the data just generated. 

rarediffs<-map_dfr(c(-1,0,1), function(m){
  map_dfr(1:nreps, function(rn){
    map_dfr(floor(10^seq(2,5,.05)), function(inds){
        #compute differences for each kind of estimator
      sdists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(samp), method="manhattan"))
      edists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(chaoest), method="manhattan"))
      cdists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(cover), method="manhattan"))
      # clunkily give the vectors names
      names(cdists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      names(edists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      names(sdists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      #return as a d.f. 
      return(bind_rows(data.frame(t(edists), l=m, size=inds, reps=rn, meth="chao"), data.frame(t(cdists), l=m, size=inds, reps=inds, meth="coverage"), data.frame(t(sdists), l=m, size=inds, reps=rn, meth="size")))
      })
    })
})

#tidy up 
gthrd<-rarediffs %>% 
  gather(diff_btwn, val, 1:6)

# This is probably nota helpful way to visualize how far off they are
pdf(file="figures/eval_methods.pdf")
gthrd %>% 
  ggplot(aes(size, val, color=meth)) +
  facet_wrap(~l+diff_btwn, ncol=6)+
  scale_x_log10()+
  geom_jitter(size=.1, alpha=0.5, height=0)+
  theme_classic() 
dev.off()

#compute RMSE against true differences in diversity between comms
rmses<-map_dfr(floor(10^seq(2,5,.05)), function(inds){
  sqe<-gthrd %>% filter(size==inds) %>% left_join(k) %>% mutate(sqdiff=(trueval-val)^2, method=meth)

  evalu<-sqe %>% group_by(l, method) %>% summarize(rmse=sqrt(mean(sqdiff)))
  return(data.frame(evalu, size=inds))
})

rmses<-rmses %>% left_join(data.frame(l=c(-1,0,1), hill=c("Hill-Simpson", "Hill-Shannon", "Richness")))

rmses$hill<-factor(rmses$hill, levels=c("Hill-Simpson", "Hill-Shannon", "Richness"))

pdf(file="figures/sample_a_lot_use_coverage.pdf")
rmses %>% ggplot(aes(size, rmse, color=method, shape=method))+
  geom_point(alpha=0.5, size=.75)+
  geom_line(alpha=0.5, size=0.5)+
  facet_wrap(~hill)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  labs(x="sample size (individuals)", y="RMSE in predicting pairwise differences \nin log(diversity) between communities")
dev.off()

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

