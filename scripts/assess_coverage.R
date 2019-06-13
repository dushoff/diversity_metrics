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

diffs<-map_dfr(c(-1,0,1), function(l){
    dat<-out %>% filter(l==l)
    data.frame(three_1=dat[which(dat$comm==3),"logdiv"]-dat[which(dat$comm==1),"logdiv"],three_2=dat[which(dat$comm==3),"logdiv"]-dat[which(dat$comm==2),"logdiv"], two_1=dat[which(dat$comm==2),"logdiv"]-dat[which(dat$comm==1),"logdiv"], four_1=dat[which(dat$comm==4),"logdiv"]-dat[which(dat$comm==1),"logdiv"],four_2=dat[which(dat$comm==4),"logdiv"]-dat[which(dat$comm==2),"logdiv"], four_3=dat[which(dat$comm==4),"logdiv"]-dat[which(dat$comm==3),"logdiv"],l=l)
    })
diffs

nc<-60
plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands
nreps<-100
rarefs<-future_map_dfr(1:nreps, function(reps){
        map_dfr(10^seq(1,5,.25), function(size){
            rare<-lapply(1:4, function(com){subsam(get(paste("comm", com, sep="")),size )})
            names(rare)<-1:4
            covdivs<-estimateD(rare, base="coverage")
            map_dfr(1:4, function(com){
                map_dfr(c(-1,0,1), function(l){
                    samp<-dfun(rare[[com]], l=l)
                    chaoest<-Chao_Hill_abu(rare[[com]], q=1-l)
                    return(data.frame(samp=samp, chaoest=chaoest, cover=covdivs[which(covdivs$site==com&covdivs$order==1-l), "qD"], coverage=covdivs[which(covdivs$site==com&covdivs$order==1-l), "SC"], l=l, size=size, comm=com, reps=reps))
                
            })
        })
    })
})

#write data to file

write.csv(raref, file="data/coverage_vs_others.csv", row.names=F)
##################
# rough plot to visualize
rarefs %>% 
    gather(etype, est, chaoest, samp) %>% 
    ggplot(aes(x=size, y=est, color=as.factor(paste(etype, comm)), fill=as.factor(paste(etype, comm))))+
        geom_smooth()+
        scale_y_log10()+
        scale_x_log10()+
        facet_wrap(~l, scales="free")+
        theme_classic()


