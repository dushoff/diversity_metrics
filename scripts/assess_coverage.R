##### code, finally, to compare asymptotic and 3 ways of doing rarefaction. 

library(tidyverse)
# library(mobsim) #for simulating SADs with sim_sad
library(furrr) #does parallelization
library(iNEXT) # Chao's package for doing coverage etc.
library(scales)
library(multidplyr) #also does parallelization. I'm not sure it's the right way to go but leaving it for now
library(viridis) # Dylan found new colorblind friendly colors
source("scripts/helper_funs/estimation_funs.R")
# source("scripts/helper_funs/read_tcsv.R")
source("scripts/helper_funs/uniroot_gamma_and_lnorm.R")
library(gtools)

# #load microbial data used in Haegeman 2013
# 
# haegdat <- read.tcsv("data/Haegeman_data.csv") 
# # deal with NAs added in previous line
# haegdat[is.na(haegdat)]<-0
# # #remove first sample, much smaller
# # haegdat<-haegdat[,-1]
# # #repeat, last is bad too, relatively
# # haegdat<-haegdat[,-8]
# #seems like we get crazy stuff if we use wildly incomparable things. So stick with the four soil communities, see if it works at all. 
# 
# haegdat<-haegdat[,2:5]
# #try doubling haegdat values to get 100% coverage by definition
# haegdat<-haegdat*2
# 
# # #make communities. 3=1+2. 4=2+2. 1 more even. 
# # comm1<-as.numeric(sim_sad(s_pool=120, n_sim=1e6, sad_coef=list(cv_abund=5)))
# # comm2<-as.numeric(sim_sad(s_pool=120, n_sim=1e6, sad_coef=list(cv_abund=9)))
# # comm3<-c(comm1, comm2)
# # comm4<-(c(comm2, comm2))
# # 
# # mikedat<-data.frame(comm1=c(comm1, rep(0,120)), comm2=c(comm2, rep(0,120)), comm3=comm3, comm4=comm4)
# # write.csv(mikedat, "data/mikedat.csv", row.names=F)
# 
# 
# #make a set of lower diversity communities
# # comm1<-as.numeric(sim_sad(s_pool=30, n_sim=1e6, sad_coef=list(cv_abund=5)))
# # comm2<-as.numeric(sim_sad(s_pool=30, n_sim=1e6, sad_coef=list(cv_abund=9)))
# # comm3<-c(comm1, comm2)
# # comm4<-(c(comm2, comm2))
# # 
# # lowdat<-data.frame(comm1=c(comm1, rep(0,30)), comm2=c(comm2, rep(0,30)), comm3=comm3, comm4=comm4)
# # 
# # write.csv(lowdat, "data/lowdat.csv", row.names=F)
# 
# mikedat<-read.csv("data/mikedat.csv")
# lowdat<-read.csv("data/lowdat.csv")
# 
# look at differences on log scale


###################################
# simulate new comms and sample per checkplot paper; ish.
# 
SADs_list<-map(c("lnorm", "gamma"), function(distr){
    map(c(.125,.25,.5), function(simp_Prop){
      fit_SAD(rich = 50, simpson = simp_Prop*50, dstr = distr)
    })
})

gettwo<-function(myrow, ind, target){
  cSAD=c(target[[ind[myrow,1]]][[3]],target[[ind[myrow,2]]][[3]])
}

guideSADs<-map(SADs_list, function(distr){
  
  lits=map(distr, function(x){
    littleSAD=x[[3]]
  })
  bigs=map(1:6, function(combo){
    ind=combinations(3,2, repeats.allowed=T)
    bigSAD=gettwo(combo, ind, distr)
    return(bigSAD)})
  return(flatten(list(lits, bigs)))
})

# guideSADs
#beware of using variables this way.. this loop was for comms 1-4
# out<-map_dfr(1:4, function(comm){
#     map_dfr(c(-1,0,1), function(l){
#         data.frame(logdiv=log(dfun(get(paste("comm", comm, sep="")), l=l)), comm=comm, l=l)
#     })
# })


#get true diffs

td<-function(mydat){
  out<- map_dfr(c(-1,0,1), function(l){
          map_dfr(1:length(mydat), function(comm){
            data.frame(logdiv=log(dfun(as.numeric(unlist(mydat[[comm]])), l=l)), comm=comm, l=l)
            # data.frame(logdiv=log(dfun(mydat[,comm], l=l)), comm=comm, l=l)
    })
  })



  k<-map_dfr(c(-1,0,1), function(m){
    dists<-out %>% filter(l==m) %>% do(data.frame(divdis=combn(.$logdiv, 2, diff)))
    db<-unite(data.frame(t(combn(1:length(mydat),2))), db)[,1]
    return(data.frame(dists, diff_btwn=db, l=m))
  
  })
  return(k)
}


td(guideSADs[[1]])
################################################################################################
###### This is a giant routine that will take a long time and tons of compute resources ########
################################################################################################

#set number of cores manually. This 64 is for running on Annotate after checking that no other big users with top
nc<-50
plan(strategy = multiprocess, workers = nc) #this is telling the computer to get ready for the future_ commands
# one rep takes a long time on one fast core. I think estimateD might be the slow function. 
nreps<-200
# maxi<-5 #max sample size=10^maxi
mini<-1.5
maxi<-4.5




assesscov<-function(mydat){future_map_dfr(1:nreps, function(reps){
  map_dfr(floor(10^seq(mini,maxi,.1)), function(inds){ #sample sizes
    mySeed<-10#1000*runif(1)
    # set.seed(mySeed)
    # set.seed(131.92345)
    # inds<-223
    rare<-lapply(1:length(mydat), function(com){sample_infinite(mydat[[com]], inds)}) #rarefy each community
    names(rare)<-1:length(mydat)
    covdivs<-tryCatch(estimateD(rare, base="coverage"), error=function(e) data.frame(site=rep(1:length(mydat),3), order=rep(c(-1,0,1), length(mydat)), qD=rep(mySeed, 3*length(mydat)), SC=rep(mySeed, 3*length(mydat)))) #use iNEXT::estimateD to compute expected Hill diversities for equal coverage
    map_dfr(1:length(mydat), function(com){ #then loop over communities again, b/c going to return one row for each combination of sample size, community, hill exponent
      map_dfr(c(-1,0,1), function(ell){ #hill exponents
        samp<-dfun(rare[[com]], l=ell) #compute sample diversity
        chaoest<-tryCatch(Chao_Hill_abu(rare[[com]], q=1-ell), error=function(e) "whoops") #compute asymptotic estimator; Chao uses q=1-l
        return(tryCatch(data.frame(samp=samp, chaoest=chaoest
                                   , cover=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "qD"] #this is diversity
                                   , coverage=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "SC"] #this is coverage estimate
                                   , cov_size=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "m"] #this is coverage estimate
                                   , cov_size=covdivs[which(covdivs$site==com&covdivs$order==1-ell), "m"] #this is the size associated with that coverage estimate
                                   , l=ell, size=inds, comm=com, reps=reps) #not sure this error thing is necessary but seems conservative to keep it

                        , error=function(e) data.frame(samp=samp, chaoest=chaoest, cover=NA, coverage=NA, cov_size=NA, l=ell, size=inds, comm=com, reps=reps)))
      })
    })
  })
})
}

# does each rarefaction using parallel set up above and writes data to disk
# map(c("mikedat", "haegdat", "lowdat"), function(dat){
#   write.csv(assesscov(get(dat)), file=paste("data/",dat, nreps, ".csv", sep=""), row.names=F)
#   })
map(1:2, function(dist){
  write.csv(assesscov(guideSADs[[1]]), file=paste("data/", c("lnorm", "gamma")[dist], ".csv", sep=""), row.names=F)
})
# write.csv(assesscov(mikedat), file="data/mikedat500.csv", row.names=F)
#creates a list for the true differences, a dataframe for each dataset
karr<-map(c("mikedat", "haegdat","lowdat" ), function(dat){
  td(get(dat))
})
names(karr)<-c("parametric_SAD","empirical_SAD", "small_SAD")
karr<-map_dfr(karr, bind_rows, .id="SAD")




#replaces diversities with log diversities
empirical_SAD<-read.csv("data/fromR/haegdat1000.csv", stringsAsFactors = F)
parametric_SAD<-read.csv("data/fromR/mikedat1000.csv", stringsAsFactors = F)

small_SAD<-read.csv("data/fromR/lowdat1000.csv", stringsAsFactors = F)

rarefs<-bind_rows(empirical_SAD,parametric_SAD, small_SAD,.id="SAD")
rarefs$SAD[rarefs$SAD=="1"]<-"empirical_SAD"
rarefs$SAD[rarefs$SAD=="2"]<-"parametric_SAD"
rarefs$SAD[rarefs$SAD=="3"]<-"small_SAD"


#rarefs<-bind_rows(empirical_SAD,parametric_SAD, .id="SAD")
rarefs<- map_dfr(c("empirical_SAD", "small_SAD", "parametric_SAD"), function(dat){
  datr<-get(dat)
  datr$size<-as.factor(as.character(datr$size))
  datr<-datr %>% group_by(size, comm) %>% summarize(meancov=mean(cov_size, na.rm=T), meanobs=mean(samp)) %>%  ungroup()%>% group_by(size) %>% #filter(meancov==min(meancov)) %>%
    mutate(cRank=min_rank(meancov), dRank=min_rank(meanobs)) %>% select(size, comm, cRank, dRank) %>% right_join(datr)
  return(datr)
}, .id="SAD")


rarefs$SAD[rarefs$SAD=="1"]<-"empirical_SAD"
rarefs$SAD[rarefs$SAD=="2"]<-"small_SAD"
rarefs$SAD[rarefs$SAD=="3"]<-"parametric_SAD"

#put cap on indiiduals at 10^4
maxi<-4
#takes about 7 mins like this with 7 cores
no_cores_to_use<-7
makeRmses<-function(rarefs){
    rarefsl<-rarefs %>% mutate(chaoest=log(chaoest), samp=log(samp), cover=log(cover))
    # indx<-ifelse(ds=="m", 1,2)
    
    #################
    # summarize differences; this is a little slow, maybe parellelize with nest() and future_map rather than using do.
    # actually parallelized with multidplyr. 
    
    clust<-new_cluster(no_cores_to_use) #this initiates a multidplyr cluster
    cluster_library(clust, "tidyverse") #this seems inefficient, but it has to load the package for each worker separately. Maybe furrr is doing this too but silently. Anwyays, it's maybe 5x faster when using 7 cores; that seems like a big improvement. 
    #It took about 6-10 minutes in series and 36 seconds this way. 
    start<-Sys.time()
    rarediffs<-rarefsl %>% 
      gather(meth, esti, samp, chaoest, cover) %>%   
      group_by(l, size, reps,  meth, SAD) %>%
      partition(clust) %>% 
      do(diff_btwn=data.frame(t(combn(.$comm, m = 2))) %>% unite(sitio) %>% pull(sitio)
         , diffs=combn(.$esti, m=2, diff)
         # , which(c(separate(sitio))==
         , cDiff=combn(.$cov_size, m=2, diff)
         
      ) %>% 
      collect() %>% 
      unnest()
    
    took<-Sys.time()-start
    print(took)
    
    
    
    ## I think I fixed things so that k and rarediffs should have differences of the same sites in the same order for robust comparisons. 
    ##compute RMSE against true differences in diversity between comms
    rmses<-future_map_dfr(floor(10^seq(mini,maxi,.05)), function(inds){
      (sqe<-rarediffs %>% filter(size==inds) 
        %>% left_join(karr) #by=c("l"="m", "diff_btwn"="diff_btwn") 
        %>% mutate(sqdiff=(divdis-diffs)^2, method=meth)
        %>% mutate(rawdiff=divdis-diffs)
        %>% mutate(bias=-1*sign(cDiff*diffs))
       )
      
      evalu<-sqe %>% 
        group_by(l, method, SAD) %>% 
        summarize(rmse=sqrt(mean(sqdiff, na.rm=TRUE))) %>% 
        left_join(sqe) %>%  
        group_by(l, diff_btwn, method, SAD, rmse) %>% 
        summarize(biascheck=mean(rawdiff, na.rm=T), biasdir=mean(bias, na.rm=T))

      return(data.frame(evalu, size=inds))
    })
    
    #just rename for human legibility
    rmses<-rmses %>% left_join(data.frame(l=c(-1,0,1), hill=c("Hill-Simpson", "Hill-Shannon", "Richness")))
    # set factor levels for plot ordering
    rmses$hill<-factor(rmses$hill, levels=c("Hill-Simpson", "Hill-Shannon", "Richness"))
    return(rmses)
}

rmses<-makeRmses(rarefs)
rmses

pdf(file="figures/which_direction_for_bias.pdf")
rmses %>%  
  mutate(biascheck_correct=c("no", "maybe", "yes")[2-sign(biascheck*biasdir)]) %>% 
  unite(BS, SAD,biascheck_correct) %>% 
  mutate(BS=factor(BS, labels=c("No ", "Yes ", "No", "Yes"))) %>% 
  filter(as.numeric(size)<500) %>% 
  ggplot(aes(BS, abs(biascheck)))+
    geom_jitter(width=0.25,height=0, alpha=0.8, aes(color=as.numeric(size)))+
    theme_classic()+
    facet_grid(method~hill)+
    labs(x="bias sign predicted by which sample has lower coverage", y="magnitutde of bias")+
    ylim(c(0,1)) +
    geom_text(aes(y=.85, label=tot)
            , data=(rmses %>%
                      mutate(biascheck_correct=c("no", "maybe", "yes")[2-sign(biascheck*biasdir)]) %>%
                      unite(BS, SAD,biascheck_correct) %>% 
                      mutate(BS=factor(BS, labels=c("No ", "Yes ", "No", "Yes"))) %>% 
                      filter(as.numeric(size)<500) %>%
                      group_by(method, hill, BS) %>%
                      summarize(tot=round(100*n()/84)))) +
    guides(color=guide_legend("sample size \n(individuals)"))
           #+
  #theme(axis.text.x=element_text(angle=90))
dev.off()


View(rmses %>%  
  mutate(biascheck_correct=c("no", "maybe", "yes")[2-sign(biascheck*biasdir)]) %>% 
  unite(BS, SAD,biascheck_correct) %>% 
  mutate(BS=factor(BS, labels=c("No", "Yes", "No", "Yes"))) %>% 
  filter(as.numeric(size)<500) %>% 
  group_by(diff_btwn, size, hill) %>% 
  summarize(n_distinct(as.numeric(biascheck))))
  


rarefs$size<-as.character(rarefs$size)
rmses$size<-as.character(rmses$size)
OBs<-rmses%>% filter(sign(biascheck*biasdir)==-1, method=="samp", hill=="Richness") %>% left_join(rarefs)
head(OBs)
View(OBs)

hist(rmses$biascheck)

oddBallMeans<-OBs %>% separate(diff_btwn, c("a", "b")) %>% filter(comm==a|comm==b) %>% unite( diff_btwn, a,b) %>%  group_by(method, SAD, size, hill, comm, diff_btwn) %>% summarize_all(mean)

View(oddBallMeans)
OBs$diff_btwn
View(oddBallMeans)
>>>>>>> 406afbeffb2eb3284fb5cc214e991dcdea5fd0d3
## Show RMSE as a function of sample size for each hill number, each method with different color/point.
# pdf(file="figures/sample_a_lot_use_coverage.pdf")
# rmses$method<-c("asymptotic estimator", "coverage-based rarefaction", "size-based rarefaction")
rmses$method<-factor(rmses$method, levels=c("chaoest", "samp", "cover"), labels=c("asymptotic estimator", "size-based rarefaction", "coverage-based rarefaction"))
rmses
#################
#try to look at biase

pdf(file="figures/bias_by_pair_and_size.pdf")
rmses %>% filter(size %in% floor(10^seq(mini,4,0.2))) %>% 
  ggplot(aes(size, (exp(biascheck)-1)*100, color=method, shape=method))+
  geom_hline(yintercept=0)+
  geom_point(alpha=0.8, size=2)+
  theme_classic()+
  facet_grid(diff_btwn~hill)+
  scale_color_viridis(discrete=T)+
  labs(y="average % deviation from true diversity ratio between communities")+
  scale_x_log10()
dev.off()

pdf(file="figures/RMS_bias.pdf")
rmses %>% 
  group_by(method, hill) %>% 
  summarize(most_biased=(sqrt(mean(exp(biascheck)^2))-1)*100, ms_sd=sqrt(sd((biascheck)^2))) %>% 
  ggplot(aes(method, most_biased, color=method, shape=method))+
  geom_point(size=5)+
  # geom_errorbar(aes(ymin=most_biased-1.96*ms_sd, ymax=most_biased+1.96*ms_sd))+
  facet_wrap(~hill, nrow=3, scale="free")+
  scale_color_viridis(discrete=T)+
  labs(y="RMS % bias across community pairs and sample sizes")+
  theme_classic()
dev.off()

assess_covPlot<-rmses %>% ggplot(aes(size, rmse, color=method, shape=method))+
    geom_point(data=rmses %>% filter(size %in% floor(10^seq(mini,maxi,0.2))),size=2)+
    geom_line()+
    facet_grid(SAD~hill)+
    scale_x_log10(labels = trans_format("log10", math_format(10^.x)), limits=c(100,10000))+
    theme_classic()+
    scale_color_viridis(discrete=T)+
    ylim(0,0.65)+
    # theme(guide_legend(title="standardization\nmethod"))+
    labs(x="sample size (individuals)", y="RMSE in predicting pairwise differences \nin log(diversity) between communities", shape="standardization\nmethod", color="standardization\nmethod")#}
    

pdf(width=9, height=6, file="figures/why_coverage.pdf")
assess_covPlot
dev.off()

rarediffsm %>% left_join(karr[[1]], by=c("l"="m", "diff_btwn"="diff_btwn")) %>% filter(l==-1) %>% ggplot(aes(size, diffs))+geom_point()+theme_classic()+facet_grid(meth~diff_btwn)+geom_hline(aes(yintercept=divdis))

#############
# find intersection
findint <- rmses %>% spread(method, rmse) %>%  group_by(hill, size) %>% mutate(chaowins = (samp-chaoest) > 0) %>% filter(hill == "Richness")
View(findint)
#crossing around 446

rarefsl %>% filter(size == 251) %>% ggplot(aes(coverage)) +
  geom_histogram() +
  facet_wrap(~comm)

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




######################################
# need to look at stuff and think about it here. not sure what files I saved. 
md500<-read.csv("data/mikedat500.csv")
head(md500)
