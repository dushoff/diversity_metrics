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

k

head(rarefs)

?dist
out
l

diffs<-map_dfr(c(-1,0,1), function(l){
    dat<-out %>% filter(l==l)
    data.frame(three_1=dat[which(dat$comm==3),"logdiv"]-dat[which(dat$comm==1),"logdiv"],three_2=dat[which(dat$comm==3),"logdiv"]-dat[which(dat$comm==2),"logdiv"], two_1=dat[which(dat$comm==2),"logdiv"]-dat[which(dat$comm==1),"logdiv"], four_1=dat[which(dat$comm==4),"logdiv"]-dat[which(dat$comm==1),"logdiv"],four_2=dat[which(dat$comm==4),"logdiv"]-dat[which(dat$comm==2),"logdiv"], four_3=dat[which(dat$comm==4),"logdiv"]-dat[which(dat$comm==3),"logdiv"],l=l)
    })
diffs

nc<-60
plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands
nreps<-60
rarefs<-future_map_dfr(1:nreps, function(reps){
       map_dfr(10^seq(2,5,.25), function(size){
            rare<-lapply(1:4, function(com){subsam(get(paste("comm", com, sep="")), size)})
            names(rare)<-1:4
            covdivs<-estimateD(rare, base="coverage")
            map_dfr(1:4, function(com){
                map_dfr(c(-1,0,1), function(m){
                    samp<-dfun(rare[[com]], l=m)
                    chaoest<-Chao_Hill_abu(rare[[com]], q=1-m)
                    return(tryCatch(data.frame(samp=samp, chaoest=chaoest, cover=covdivs[which(covdivs$site==com&covdivs$order==1-m), "qD"], coverage=covdivs[which(covdivs$site==com&covdivs$order==1-m), "SC"], l=m, size=size, comm=com, reps=reps), error=function(e) data.frame(samp=samp, chaoest=chaoest, cover="err", coverage="err", l=m, size=size, comm=com, reps=reps)))
            })
        })
    })
})



#write data to file

write.csv(rarefs, file="data/coverage_vs_others.csv", row.names=F)

# rarefsl<-rarefs %>% mutate(choaest=log(chaoest), samp=log(samp), cover=log(cover))

#this is an embarassing block of code
rarediffs<-map_dfr(c(-1,0,1), function(m){
  map_dfr(1:nreps, function(rn){
    map_dfr(10^seq(2,5,.25), function(inds){
      sdists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(samp), method="manhattan"))
      edists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(chaoest), method="manhattan"))
      cdists<-as.numeric(dist(rarefsl %>% filter(l==m, size==inds, reps==rn) %>% select(cover), method="manhattan"))
      names(cdists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      names(edists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      names(sdists)<-c("onetwo","onethree", "onefour", "twothree", "twofour", "threefour")
      return(bind_rows(data.frame(t(edists), l=m, size=inds, reps=rn, meth="chao"), data.frame(t(cdists), l=m, size=m, reps=inds, meth="coverage"), data.frame(t(sdists), l=m, size=inds, reps=rn, meth="size")))
      })
    })
})

head(rarediffs)
pdf(file="figures/eval_methods.pdf")
rarediffs %>% 
  gather(val, diff_btwn, 1:6) %>% 
  ggplot(aes(inds, val, color=meth)) +
  facet_wrap(~diff_btwn)+
  geom_point(alpha=0.2)+
  theme_classic() 

dev.off()
##################
# rough plot to visualize
rarefs %>% 
    gather(etype, est, chaoest, samp, cover) %>% 
    ggplot(aes(x=size, y=est, color=as.factor( comm), fill=as.factor(comm)))+
        geom_smooth()+
        scale_y_log10()+
        scale_x_log10()+
        facet_wrap(~etype+l, scales="free")+
        theme_classic()


