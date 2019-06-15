##### code, finally, to compare asymptotic and 3 ways of doing rarefaction. 

library(tidyverse)
library(mobsim)
library(furrr)
library(iNEXT)

source("scripts/helper_funs/estimation_funs.R")


#make communities. 3=1+2
comm1<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=5)))
comm2<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=9)))
comm3<-c(comm1, comm2)
comm4<-(c(comm2, comm2))

# look at differences on log scale

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

k
nc<-64
plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands
nreps<-500
rarefs<-future_map_dfr(1:nreps, function(reps){
       map_dfr(floor(10^seq(2,5,.05)), function(inds){
            rare<-lapply(1:4, function(com){subsam(get(paste("comm", com, sep="")), inds)})
            names(rare)<-1:4
            covdivs<-estimateD(rare, base="coverage")
            map_dfr(1:4, function(com){
                map_dfr(c(-1,0,1), function(m){
                    samp<-dfun(rare[[com]], l=m)
                    chaoest<-Chao_Hill_abu(rare[[com]], q=1-m)
                    return(tryCatch(data.frame(samp=samp, chaoest=chaoest, cover=covdivs[which(covdivs$site==com&covdivs$order==1-m), "qD"], coverage=covdivs[which(covdivs$site==com&covdivs$order==1-m), "SC"], l=m, size=inds, comm=com, reps=reps), error=function(e) data.frame(samp=samp, chaoest=chaoest, cover="err", coverage="err", l=m, size=inds, comm=com, reps=reps)))
            })
        })
    })
})



#write data to file

write.csv(rarefs, file="data/coverage_vs_others.csv", row.names=F)

rarefsl<-rarefs %>% mutate(chaoest=log(chaoest), samp=log(samp), cover=log(cover))

#this is an embarassing block of code
rarediffs<-map_dfr(c(-1,0,1), function(m){
  map_dfr(1:nreps, function(rn){
    map_dfr(floor(10^seq(2,5,.25)), function(inds){
      sdists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(samp), method="manhattan"))
      edists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(chaoest), method="manhattan"))
      cdists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(cover), method="manhattan"))
      # print(c(m, rn, inds))
      # print(max(cdists))
      # print(max(edists))
      # print(max(sdists))
      names(cdists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      names(edists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      names(sdists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      return(bind_rows(data.frame(t(edists), l=m, size=inds, reps=rn, meth="chao"), data.frame(t(cdists), l=m, size=inds, reps=inds, meth="coverage"), data.frame(t(sdists), l=m, size=inds, reps=rn, meth="size")))
      })
    })
})

gthrd<-rarediffs %>% 
  gather(diff_btwn, val, 1:6)


head(gthrd)
pdf(file="figures/eval_methods.pdf")
gthrd %>% 
  ggplot(aes(size, val, color=meth)) +
  facet_wrap(~l+diff_btwn, ncol=6)+
  scale_x_log10()+
  geom_jitter(size=.1, alpha=0.5, height=0)+
  theme_classic() 
dev.off()

sqe<-gthrd %>% filter(size>400) %>% left_join(k) %>% mutate(sqdiff=(trueval-val)^2)

evalu<-sqe %>% group_by(l, meth) %>% summarize(rmse=sqrt(mean(sqdiff)))
evalu




summary(rarediffs[which(rarediffs$meth=="coverage"), "size"])
##################
# rough plot to visualize
rarefs[which(rarefs$comm==1), "comm"]<-"a"
rarefs[which(rarefs$comm==2), "comm"]<-"b"
rarefs[which(rarefs$comm==3), "comm"]<-"a+b"
rarefs[which(rarefs$comm==4), "comm"]<-"a+a"

pdf(file="figures/rough_rae_fig.pdf")
rarefs %>% 
    gather(etype, est, chaoest, samp, cover) %>% 
    ggplot(aes(x=size, y=est, color=as.factor( comm), fill=as.factor(comm)))+
        geom_smooth()+
        scale_y_log10()+
        scale_x_log10()+
        facet_wrap(~etype+l, scales="free")+
        labs(y="estimated diversity")+
        theme_classic()
dev.off()

