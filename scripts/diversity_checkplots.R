# script to make checkplots for Chao's variance estimators for diversity estimates. Also creates non-checkplot figure of CI coverage for Chao1 for the conceptual guide.

#############
#load functions and packages
library(furrr)#parallelization
source("scripts/helper_funs/uniroot_gamma_and_lnorm.R")
# source("/Rstudio_Git/Dushoff_diversity/scripts/helper_funs/radplot.R")
# source("scripts/helper_funs/prettify.R")
library(scales)#trans_breaks
# library(cowplot) #sometimes nice stuff for multipanel ggplotting
# invlogit<-arm::invlogit
#library(vegan) # for fisherfit
#library(MASS) # for fitdistr
# library(scales) #for function muted
select<-dplyr::select

########################################
#simulate communities 
# #start with JD comms
# mini<-100
# com1 <- c(1, 1, 1, 1)
# com2 <- c(1, rep(1/(mini-1), mini))
# #add a more naturalistic one
# com3<-as.numeric(sim_sad(s_pool=60, n_sim=100000))
# #this is supersampling
# o1<-sample(1:length(com1), prob=com1, size=inds, replace=T)
# o2<-sample(1:length(com2), prob=com2, size=inds, replace=T)



# #make a community for User's Guide
# usersguide<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=5)))
# 
# 
# #three communities with richness ~60, and different skew
# com1<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=2)))
# com2<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=5)))
# com3<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=10)))

# simulate communities with fit_SAD, basically isntantaneous

SADs_list<-map(c("lnorm", "gamma"), function(distr){
  map(c(100, 200), function(rich){
    map(c(.15,.25,.5,.75,.85), function(simp_Prop){
      fit_SAD(rich = rich, simpson = simp_Prop*rich, dstr = distr)
    })
  })
})




#quick summary to see how distributional assumption affects Shannon
see_Shannon <- map_dfr(SADs_list, function(dst){
  map_dfr(dst, function(R){
    map_dfr(R, function(S){
      return("summaryStats" =data.frame(t(c(S$distribution_info, S$community_info))))
    })
  })
})

#well, not much! But shannon is always higher with lnorm
pdf("figures/Shannon_higher_with_lnorm.pdf")
see_Shannon %>% 
  ggplot(aes(as.numeric(as.character(Hill.Simpson))/as.numeric(as.character(richness))
             , as.numeric(as.character(Hill.Shannon)), color=distribution, shape=richness))+
    geom_point()+theme_classic()+scale_y_log10() +
    scale_x_log10()+
    labs(x="Hill-Simpson as fraction of richness", y="Hill-Shannon")
dev.off()

##############
# make rank abundance distributions

myabs<-map_dfr(flatten(flatten(SADs_list)), function(x)data.frame(ab=x$rel_abundances), .id="SAD")

remsub <- function(variable, value){
  return(gsub("_"," ",value))}

pdf("figures/RAD_for_extreme_SADs_rich_100.pdf")
myabs %>% left_join(data.frame(
  SAD=as.character(1:20)
  , skew=factor(c("uneven", "int", "int","int", "even"), levels=c("uneven", "int", "even"))
  , dist=factor(c(rep("lognormal", 10), rep("gamma", 10)), levels=c("lognormal", "gamma"))
)) %>% 
  mutate(abD=paste(dist, skew)) %>% 
  group_by(SAD, abD, dist, skew) %>% 
  mutate(abrank=min_rank(desc(ab)), log_relative_abundance=log(ab), relative_abundance=ab) %>% 
  gather(scl, rel_abund, relative_abundance, log_relative_abundance )%>% 
  filter(SAD %in% c("1","11","5","15")) %>% 
  ggplot(aes(abrank, rel_abund, color=dist))+
    geom_point(alpha=0.5, size=1)+
    geom_line(size=.4, alpha=0.5)+
    theme_classic()+
    theme(text=element_text(size=16))+
    labs(x="abundance rank", y="", color="", shape="")+
    facet_grid(fct_rev(scl)~skew, scales="free", switch="y", labeller=remsub)
dev.off()               


# pdf(height=2, width=6, file="figures/simssads.pdf")
# par(mfrow=c(1,3))
# plot(1:length(com1), com1, xlab="",  ylab="species abundance", type="line", ylim=c(0,26000))
# plot(1:length(com2), com2, xlab="species rank", ylab="", type="line", ylim=c(0,26000))
# plot(1:length(com3), com3, xlab="", ylab="", type="line", ylim=c(0,26000) )
# dev.off()

asab<-function(namevec){as.numeric(table(namevec))}



#gets slightly closer than obs
# show1<-checkplot(com1, l=0, inds=150, reps=1000)

####################################
#set up parallelization for large computations

#set # cores
nc<-7#per Rob's recommendation


plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands

########################
# # function to generate data for checkplots for fixed communities
# checkplot<-function(abs, B=Bnum, l, inds, reps){
#   td<-dfun(abs, l) #compute true diversity
#   #truemu_n<-mean(replicate(B,dfun(subsam(abs, inds),l)))
#   future_map_dfr(1:reps,function(x){
#     obs<-subsam(abs, size=inds) #subsample true community within each replicate
#     chaotile<-checkchao(obs, B, l, td) #then do B bootstrap samples for the augmented community based on that sample
#     return(chaotile=data.frame(qtile=chaotile[1], truediv=chaotile[2], chaoest=chaotile[3], obsD=chaotile[4], l=l, inds=inds, reps=reps))
# 
#   })
# }

# for infinite community, generated by fit_SAD (SAD is the name of that list object)
checkplot_inf<-function(SAD, B=2000, l, inds, reps){
  hillname<-ifelse(l==-1, "Hill-Simpson", ifelse(l==0, "Hill-Shannon", "richness"))
  td<-SAD$community_info[hillname] #grab true diversity from SAD object
  #truemu_n<-mean(replicate(B,dfun(subsam(abs, inds),l)))
  future_map_dfr(1:reps,function(x){
    # obs<-subsam(abs, size=inds) #subsample true community within each replicate
    
    obs <- sample_infinite(SAD$rel_abundances,size=inds) #subsample the whole community with # individuals=size
    chaotile<-checkchao(obs, B, l, td) #then do B bootstrap samples for the augmented community based on that sample
    return(chaotile=data.frame(qtile=chaotile[1], truediv=chaotile[2], chaoest=chaotile[3], obsD=chaotile[4], l=l, inds=inds, reps=reps))
    
  })
}



#########################
#This uses two functions to generate the CI and the true average sample diversity for different sample sizes for a single community

#Generates quantiles of bootstrap distribution given true diveristy, sample size, l, and true average 
#for infinite community. Note that SAD is a list generated by fit_SAD
obscp_inf <- function(l=l, size=size, SAD=SAD, B=2000, truemun=truemun...){
  sam <- sample_infinite(SAD$rel_abundances, size=size)
  data.bt = rmultinom(B,size,Bt_prob_abu(sam)) #this genenerates "bootstrapped" samples, using Chao's fancy method
  obs<-dfun(sam,l) #K. here we are just taking a single sample and computing the observed diversity
  pro = apply(data.bt,2,function(boot)dfun(boot, l)) #This had been inconsistent with intent and was doing estimator stuff where we just wanted the naive answer.
  pro_mc<-pro-mean(pro)+obs
  chaotile_mc<-sum(pro_mc<=truemun)/(B/100)
  chaotile<-sum(pro<=truemun)/(B/100)
  return(data.frame("chaotile"=chaotile, "chaotile_mc"=chaotile_mc,
                    "truemu"=truemun,  "obsD"=obs, "l"=l, "size"=size ))
}

# #for a fixed community 
# obscp<-function(l=l, size=size, dat=usersguide, B=2000, truemun=truemun...){
#   sam<-subsam(dat, size) #substample the whole community with # individuals=size
#   data.bt = rmultinom(B,size,Bt_prob_abu(sam)) #this genenerates "bootstrapped" samples, using Chao's fancy method
#   obs<-dfun(sam,l) #K. here we are just taking a single sample and computing the observed diversity
#   pro = apply(data.bt,2,function(boot)dfun(boot, l)) #This had been inconsistent with intent and was doing estimator stuff where we just wanted the naive answer.
#   pro_mc<-pro-mean(pro)+obs
#   chaotile_mc<-sum(pro_mc<=truemun)/(B/100)
#   chaotile<-sum(pro<=truemun)/(B/100)
#   return(data.frame("chaotile"=chaotile, "chaotile_mc"=chaotile_mc, "truemu"=truemun,  "obsD"=obs, "l"=l, "size"=size ))
# }

#set number of reps
reps<-5e3
Bnum<-200

####################
#run this whole thing to get sample diversity checkplot-type info for sample diversity for a single community
trycheckingobs<-function(SAD){
  map_dfr(round(10^seq(2, 5.5, 0.25)), function(size){
    map_dfr(c(-1,0,1), function(ell){
          truemun<-truemu_inf(SAD$rel_abundances, size=size, reps=reps, l=ell)
            map_dfr(1:reps, function(reps){obscp_inf(l=ell, size, SAD, truemun=truemun, B=Bnum)
            })
      })
  })
}
##### apparently this was streamlined enough to store to a single .csv while running in parallel

# future_map(1:length(flatten(flatten(SADs_list))), function(SAD){
#     write.csv(trycheckingobs(flatten(flatten(SADs_list))[[SAD]])
#               , file=paste("data/fromR/trycheckingobs_SAD_", SAD, ".csv", sep=""), row.names=F)
# })

# write.csv(trycheckingobs_R, file="data/big_richness_checkplot.csv", row.names=F)

# write.csv(trycheckingobs, file="data/fromR/trycheckingobs_with_without_mc.csv", row.names=F)


##################################
# read in data and make checkplot for sample diveristy
# trycheckingobs<-read.csv("data/fromR/trycheckingobs_with_without_mc.csv")


# trycheckingobs<-read.csv("data/fromR/trycheckingobs_with_without_mc.csv")
sample_div_cp<-future_map_dfr(1:20, function(SAD){
    newdf<-read.csv(, file=paste("data/fromR/trycheckingobs_SAD_", SAD, ".csv", sep=""))
    return(data.frame(newdf, "SAD_index"=rep(SAD, length(newdf[,1]))))
})

sample_div_cp %>% filter(size==562) %>% ggplot(aes(obsD))+geom_density()+facet_grid(SAD_index~l)
unique(sample_div_cp$size)

# sample_div_cp<-future_map_dfr(1:20, function(SAD){
#   newdf<-read.csv(file=paste("data/fromR/fromR/trycheckingobs_SAD_", SAD, ".csv", sep=""))
#   return(data.frame(newdf, "SAD_index"=rep(SAD, length(newdf[,1]))))
# })
# 

# write.csv(sample_div_cp, "data/fromR/sample_diversity_checkplots.csv", row.names=F)


# sample_div_cp<-read.csv("data/fromR/trycheckingobs_SAD_3.csv")
# sample_div_cp<-read.csv("data/fromR/sample_diversity_checkplots.csv")
head(sample_div_cp)
#compute sd_logs for these
pdf("figures/variability_in_sample_diversity_extremes.pdf")
sample_div_cp %>% 
  left_join(data.frame(l=c(-1,0,1)
                       , hill=factor(c("Hill-Simpson", "Hill-Shannon", "Richness")
                                     , levels=c("Hill-Simpson", "Hill-Shannon", "Richness")))) %>% 
  left_join(data.frame(
    SAD_index=1:20
    , skew=factor(c("uneven", "int", "int","int", "even"), levels=c("uneven", "int", "even"))
    , dist=factor(c(rep("lognormal", 10), rep("gamma", 10)), levels=c("lognormal", "gamma"))
   )) %>% 
  filter(SAD_index %in% c(1, 11, 5, 15) )%>% 
    group_by(hill, size, skew, dist, SAD_index) %>% 
    summarize(sdlog=sd(log(obsD))) %>% 
    ggplot(aes(size, sdlog, color=hill, shape=hill))+
    geom_point(alpha=0.8)+
    geom_line(alpha=0.8)+
    scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    theme_classic()+
    # geom_hline(yintercept=0.1)+
    labs(x="sample size (individuals)"
         , y="SD of log(sample diversity) under random sampling"
         , color="", shape="" )+
    facet_grid(dist~skew)+
  theme(text=element_text(size=16))
dev.off()

#####################
#checkplot figure, not used in users guide, now does checkplot without mean correction. 
# pdf(file="figures/empirical_checkplot1_nomc.pdf")
# map(c(-1,0,1), function(ell){
#     trycheckingobs %>% filter(l==ell) %>% 
#         ggplot(aes(chaotile))+
#         geom_histogram()+
#         theme_classic()+
#         facet_wrap(~size+l)
# })
# dev.off()

pdf("figures/too_many_checkplots_sample_diversity.pdf")
future_map(1:40, function(SAD){
  map(-1:1, function(ell){
    sample_div_cp %>% filter(SAD_index==SAD, l==ell) %>% 
      ggplot(aes(chaotile_mc/100))+
      geom_histogram(breaks=seq(0, 1, by=0.025))+
      theme_classic()+
      facet_wrap(~size)+
      ggtitle(paste("SAD number", SAD, "ell =",ell))
  })
})

dev.off()

pdf("figures/too_many_checkplots_sample_diversity_limited_even.pdf")
# map(1:40, function(SAD){
map(-1:1, function(ell){
  sample_div_cp %>% filter(SAD_index==9, l==ell, size %in% round(10^seq(2,3,.25))) %>% 
    ggplot(aes(chaotile_mc))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(~size)+
    ggtitle(paste("checkplot for sample Hill diveristy, ell =",ell))
})
# })

dev.off()

pdf("figures/too_many_checkplots_sample_diversity_limited.pdf")
# map(1:40, function(SAD){
map(-1:1, function(ell){
  map(1:4, function(SAD){
    sample_div_cp %>% filter(SAD_index==c(1,5,11,15)[SAD], l==ell, size %in% round(10^seq(2, 4, .5))) %>% 
      ggplot(aes(chaotile_mc/100))+
      geom_histogram()+
      theme_classic()+
      facet_wrap(~size)+
      ggtitle(paste("checkplot for sample "
                    , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell]
                    ,"; "
                    , c("lognormal; uneven","lognormal; even", "gamma; uneven", "gamma; even")[SAD] 
                    , sep=""
                    ))+
      labs(x="p-value")
  })
})

dev.off()
####################################
# set up data for users guide figures
nreps<-5e3 #this should be 5000 for now

# #df with true coverage of 95% CI
# tvcov <-trycheckingobs %>% group_by(l, size) %>% summarize(outside=1-(sum(chaotile_mc>97.5)+sum(chaotile_mc<2.5))/(nreps*100))

#relable facets by creating new factor in df
inds<-data.frame("l"=c(1,0,-1), 
                 divind=factor(c("richness", "Hill-Shannon", "Hill-Simpson"),
                                              levels=c("richness", "Hill-Shannon", "Hill-Simpson")
                               )
                 )

# tc<-left_join(tvcov, inds)
tc<-sample_div_cp %>% group_by(l, size, SAD_index) %>% summarize(outside=1-(sum(chaotile_mc>97.5)+sum(chaotile_mc<2.5))/(5000)) %>% left_join(inds)

tc

min(tc$outside)
#################################################
# this is asymptotic diversity for users guide


#####################
#uncomment to generate data for asymptotic diveristy checkplot/ CI coverage

# #set reps to 500 but outerreps to 10 for efficient use of anotate
# reps<-500
# outerreps<-10
# nc<-36#per Rob's recommendation
# plan(strategy=multiprocess, workers=nc) 

# map(1:length(flatten(flatten(SADs_list))), function(SAD){
#   map(1:outerreps, function(x){
#     ug_asy<-map_dfr(round(10^seq(2, 5.5, 0.25)), function(size){
#         map_dfr(c(-1,0,1), function(l){
#             out<-checkplot_inf(flatten(flatten(SADs_list))[[SAD]], l=l, inds=size, reps=reps)
#         })
#     })
#     write.csv(ug_asy, paste("data/SAD", SAD, "asy",  x, ".csv", sep="_"), row.names=F)
#     # return(ug_asy)
#   })
# })


#######################################
# extract data from files for use

outerreps<-10
sad_list_length<-20
getug<-future_map_dfr(1:sad_list_length, function(x){
  map_dfr(1:outerreps, function(y){
    mydf<-tryCatch(read.csv(paste("data/SAD", x, "asy",  y, ".csv", sep="_")), error=function(e){
      data.frame(qtile=NA, truediv=NA, chaoest=NA, obsD=NA, l=NA, inds=NA, reps=NA)})
    mydf<-data.frame(mydf, SAD_ind=x)
       return(mydf)
    
  })
    
})

#summarize SDlog(diversity)
sdlogs<-getug %>% gather(etype, div, chaoest, obsD )%>% group_by(l, inds, SAD_ind, etype) %>% summarize(sdlog=sd(log(div), na.rm=T), cv=sd(div, na.rm=T)/mean(div, na.rm=T))

#figure for MS possibly, showing how this works for even and uneven comms 
pdf("figures/variability_in_asymptotic_diversity_extremes.pdf")
sdlogs %>% 
  filter(etype=="chaoest") %>%
  left_join(data.frame(l=c(-1,0,1)
                       , hill=factor(c("Hill-Simpson", "Hill-Shannon", "Richness")
                                     , levels=c("Hill-Simpson", "Hill-Shannon", "Richness")))) %>% 
  left_join(data.frame(
    SAD_ind=1:20
    , skew=factor(c("uneven", "int", "int","int", "even"), levels=c("uneven", "int", "even"))
    , dist=factor(c(rep("lognormal", 10), rep("gamma", 10)), levels=c("lognormal", "gamma"))
  )) %>% 
  filter(SAD_ind %in% c(1, 11, 5, 15) )%>% 
  group_by(hill, inds, skew, dist, SAD_ind) %>% 
  ggplot(aes(inds, sdlog, color=hill, shape=hill))+
  geom_point(alpha=0.8)+
  geom_line(alpha=0.8)+
  scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  # geom_hline(yintercept=0.1)+
  labs(x="sample size (individuals)"
       , y="SD of log(asymtptotic diversity) under random sampling"
       , color="", shape="" )+
  facet_grid(dist~skew)+
  theme(text=element_text(size=16))
dev.off()


plot(sdlogs$sdlog, sdlogs$cv)
pdf(file="figures/sampling_variability_asymptotic.pdf")
sdlogs %>%
    filter(etype=="chaoest") %>%  
               ggplot(aes(inds, sdlog, color=factor(l), shape=factor(l)))+
    geom_point()+
    geom_line()+
    facet_wrap(~SAD_ind, ncol=5)+
    theme_classic()+scale_x_log10()+
    theme(axis.text.x=element_text(angle=90))+
    # geom_hline(yintercept=0.1)+
  labs(x="sample size", y="SD of log(asymptotic estimator) under random sampling")
dev.off()
#look at SDlog of estimates


pdf("figures/too_many_checkplots_asymptotic_diversity.pdf")
map(1:20, function(SAD){
  map(-1:1, function(ell){
    try(getug %>% filter(SAD_ind==SAD, l==ell) %>% 
      ggplot(aes(qtile/100))+
      geom_histogram()+
      theme_classic()+
      facet_wrap(~inds)+
      ggtitle(paste("SAD number", SAD, "ell =",ell))
    )
  })
})
dev.off()
#########################
# A few checkplots for asymptotic diversity

pdf("figures/checkplots_asymptotic_limited.pdf")
map(-1:1, function(ell){
  map(1:4, function(SAD){
    getug %>% filter(SAD_ind==c(1,5,11,15)[SAD], l==ell, inds %in% round(10^seq(2, 4, .5))) %>% 
      ggplot(aes(qtile/100))+
      geom_histogram()+
      theme_classic()+
      facet_wrap(~inds)+
      ggtitle(paste("checkplot for asymptotic "
                    , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell]
                    ,"; "
                    , c("lognormal; uneven","lognormal; even", "gamma; uneven", "gamma; even")[SAD] 
                    , sep=""
      ))+
      labs(x="p-value")
  })
})
dev.off()
#############
# make a few checkplots for draft
pdf("figures/asymptotic_richness_checkplot.pdf")
getug %>% filter(SAD_ind==6, l==1, inds %in% 10^c(2:5)) %>% 
  ggplot(aes(qtile/100))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~inds, nrow=1)+
  labs(x="p-value")+
  ggtitle("Richness = 200, Hill-Simpson=30, checkplot for asymptotic richness")

dev.off()

pdf("figures/asymptotic_Shannon_checkplot.pdf")
getug %>% filter(SAD_ind==6, l==0, inds %in% 10^c(2:5)) %>% 
  ggplot(aes(qtile/100))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~inds, nrow=1)+
  labs(x="p-value")+
  ggtitle("Richness = 200, Hill-Simpson=30, checkplot for asymptotic Hill-Shannon")

dev.off()

pdf("figures/asymptotic_Simpson_checkplot.pdf")
getug %>% filter(SAD_ind==6, l==-1, inds %in% 10^c(2:5)) %>% 
  ggplot(aes(qtile/100))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~inds, nrow=1)+
  labs(x="p-value")+
  ggtitle("Richness = 200, Hill-Simpson=30, checkplot for asymptotic Hill-Simpson")

dev.off()


pdf("figures/asymptotic_richness_checkplot_gamma.pdf")
getug %>% filter(SAD_ind==11, l==1, inds %in% 10^c(2:5)) %>% 
  ggplot(aes(qtile/100))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~inds, nrow=1)+
  labs(x="p-value")+
  ggtitle("Richness = 100, Hill-Simpson=15, checkplot for asymptotic richness")

dev.off()

pdf("figures/asymptotic_Shannon_checkplot_gamma.pdf")
getug %>% filter(SAD_ind==11, l==0, inds %in% 10^c(2:5)) %>% 
  ggplot(aes(qtile/100))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~inds, nrow=1)+
  labs(x="p-value")+
  ggtitle("Richness = 100, Hill-Simpson=15, checkplot for asymptotic Hill-Shannon")

dev.off()

pdf("figures/asymptotic_Simpson_checkplot_gamma.pdf")
getug %>% filter(SAD_ind==11, l==-1, inds %in% 10^c(2:5)) %>% 
  ggplot(aes(qtile/100))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~inds, nrow=1)+
  labs(x="p-value")+
  ggtitle("Richness = 100, Hill-Simpson=15, checkplot for asymptotic Hill-Simpson")

dev.off()

pdf("figures/asymptotic_even_Simpson_checkplot.pdf")
getug %>% filter(SAD_ind==5, l==-1, inds %in% 10^c(2:5)) %>% 
  ggplot(aes(qtile/100))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~inds, nrow=1)+
  labs(x="p-value")+
  ggtitle("Richness = 100, Hill-Simpson=85, checkplot for asymptotic Hill-Simpson")

dev.off()


asycov<-getug%>% filter(SAD_ind==7) %>% 
    group_by(l, inds) %>% 
    summarize(outside=1-(sum(qtile>97.5)+sum(qtile<2.5))/(5000)) %>% left_join(inds)


###############################
# combine sample and saymptotic to make a single multi-panel graph 
comb_cov<-bind_rows("sample diversity"=tc %>% filter(SAD_index==7) %>% rename(inds=size), "asymptotic diversity"=asycov, .id="esttype" )
#code to generate plot
#to figure out y-axis breaks:  prettify(trans_breaks(arm::logit, invlogit, n=15)(c(0.5,0.9999999)))

##########################################
# figure for Users guide to show statistical coverage for asymptotic estimators and also sample diveristy CIs
pdf(file="figures/CI_coverage_guide.pdf", height=6, width=6) #

comb_cov %>% filter(inds<=10^4) %>% 
    mutate(conserv=log(outside/(1-outside))) %>% 
    ggplot(aes(inds, outside, color=conserv, shape=esttype))+
    geom_point(size=2)+

    geom_hline(yintercept=0.95)+
    facet_grid(divind~esttype, switch="y" )+#strip.position=NULL
    theme_classic()+
    scale_shape_manual(values=c(17,15))+
    scale_color_gradient2(low="red",mid="grey55", high="cyan", limits=c(-.6,6), midpoint=2.944, breaks=c(-.6,6), labels=c(" over-confident",  " conservative"))+
    scale_y_continuous(trans="logit", limits=c(0.3, .995), breaks=c(0.3,0.5, 0.73, 0.88, 0.95, 0.98,0.99), labels=c(30,50, 73, 88, 95, 98, 99))+ #modify this so that it doesn't go quite as high
    scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
    labs(y="% chance 95% CI contains true value")+
    theme(legend.title = element_blank()
          , panel.spacing=unit(1.3, "lines")
          , axis.title.x=element_blank()
          , strip.placement.y = "outside"
          , strip.background = element_blank()
          , strip.text=element_text(face="bold")
          , legend.text=element_text(hjust=0.5)    )+
    guides(shape=F)

dev.off()


2^15

##########
# generate comms with 3 spp
nsamp<-5e2
maxr<-24

nc<-12
R_FUTURE_FORK_ENABLE<-T

plan(strategy=multiprocess, workers=nc)
library(tictoc)
tic()
myres<-future_map_dfr(1:maxr, function(rep){
  map_dfr(1:100, function(irep){
    A<-runif(1, 1/3, 1)
    B<-runif(1, (1-A)/2, 1-A)
    C<-1-A-B
    map_dfr(2^c(2:15), function(inds){
      mysams<-replicate(nsamp, sample_infinite(c(A,B,C), size=inds))
      sam_shan<-sd(log(unlist(apply(mysams, MARGIN=2, FUN=function(x){dfun(x, 0)}))), na.rm=T)
      sam_sim<-sd(log(unlist(apply(mysams, MARGIN=2, FUN=function(x){dfun(x, -1)}))), na.rm=T)
      return(data.frame(inds, sam_shan, sam_sim, A, B, C))
    })
  })
})
toc()

myres<-myres %>% rowwise(.) %>% 
  mutate(low=min(c(A,B,C)), mid=median(c(A,B,C)), high=max(c(A,B,C)), rulebreaker=sam_shan>sam_sim
         , Hill.Shannon=dfun(c(A,B,C),0), Hill.Simpson=dfun(c(A,B,C),-1))


myres %>% ggplot(aes(Hill.Shannon, Hill.Simpson, color=rulebreaker, shape=rulebreaker))+
  geom_point(alpha=0.02)+
  theme_classic()

pdf("figures/better_variability_plot.pdf")
myres %>% 
  left_join(myres %>% 
              group_by(low, mid) %>% 
              summarize(rb=sum(rulebreaker), rf=sum(!rulebreaker), purity=max(c(rb, rf))/14)) %>% 
  ggplot(aes(low/mid, mid/high, color=7.5-rf, shape=rulebreaker))+
  geom_point(alpha=0.05, size=3)+
  scale_color_gradient2()+
  guides(color=guide_colorbar("tendency to break the rules"))+
  theme_classic()
  

dev.off()

myres %>% ggplot(aes(inds, fill=rulebreaker))+geom_histogram(alpha=0.2, position="dodge")

?guides
