# script to make checkplots for Chao's variance estimators for diversity estimates. Also creates non-checkplot figure of CI coverage for Chao1 for the conceptual guide.

#############
#load functions and packages
library(tidyverse)
library(furrr)#parallelization
source("scripts/helper_funs/estimation_funs.R")
source("scripts/helper_funs/prettify.R")
library(scales)#trans_breaks
library(mobsim)#simulate communities
library(cowplot) #sometimes nice stuff for multipanel ggplotting
invlogit<-arm::invlogit
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



#make a community for User's Guide
usersguide<-as.numeric(sim_sad(s_pool=120, n_sim=1000000, sad_coef=list(cv_abund=5)))


#three communities with richness ~60, and different skew
com1<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=2)))
com2<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=5)))
com3<-as.numeric(sim_sad(s_pool=60, n_sim=100000, sad_coef=list(cv_abund=10)))

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
nc<-36#per Rob's recommendation


plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands

########################
# function to generate data for chekcplots
checkplot<-function(abs, B=2000, l, inds, reps){
  td<-dfun(abs, l) #compute true diversity
  #truemu_n<-mean(replicate(B,dfun(subsam(abs, inds),l)))
  future_map_dfr(1:reps,function(x){
    obs<-subsam(abs, size=inds) #subsample true community within each replicate
    chaotile<-checkchao(obs, B, l, td) #then do B bootstrap samples for the augmented community based on that sample
    return(chaotile=data.frame(qtile=chaotile[1], truediv=chaotile[2], chaoest=chaotile[3], obsD=chaotile[4], l=l, inds=inds, reps=reps))

  })
}

#########################
#This uses two functions to generate the CI and the true average sample diversity for different sample sizes for a single community

#Generates quantiles of bootstrap distribution given true diveristy, sample size, l, and true average
obscp<-function(l=l, size=size, dat=usersguide, B=2000, truemun=truemun...){
    sam<-subsam(dat, size) #substample the whole community with # individuals=size
    data.bt = rmultinom(B,size,Bt_prob_abu(sam)) #this genenerates "bootstrapped" samples, using Chao's fancy method
    obs<-dfun(sam,l) #K. here we are just taking a single sample and computing the observed diversity
    pro = apply(data.bt,2,function(boot)dfun(boot, l)) #This had been inconsistent with intent and was doing estimator stuff where we just wanted the naive answer.
    pro_mc<-pro-mean(pro)+obs
    chaotile_mc<-sum(pro_mc<=truemun)/(B/100)
    chaotile<-sum(pro<=truemun)/(B/100)
    return(data.frame("chaotile"=chaotile, "chaotile_mc"=chaotile_mc, "truemu"=truemun,  "obsD"=obs, "l"=l, "size"=size ))
}

#set number of reps
reps<-2500
Bnum<-200

####################
#run this whole thing to get sample diversity checkplot-type info for sample diversity for a single community
trycheckingobs<-map_dfr(round(10^seq(2, 5.5, 0.25)), function(size){
  map_dfr(c(-1,0,1), function(ell){
        truemun<-truemu(usersguide, size=size, reps=reps, l=ell)
          future_map_dfr(1:reps, function(reps){obscp(l=ell, size, usersguide, truemun=truemun, B=Bnum)
          })
    })
})
##### apparently this was streamlined enough to store to a single .csv while running in parallel

# write.csv(trycheckingobs_R, file="data/big_richness_checkplot.csv", row.names=F)

write.csv(trycheckingobs, file="data/fromR/trycheckingobs_with_without_mc.csv", row.names=F)


##################################
# read in data and make checkplot for sample diveristy
trycheckingobs<-read.csv("data/fromR/trycheckingobs.csv")

#####################
#checkplot figure, not used in users guide, now does checkplot without mean correction. 
pdf(file="figures/empirical_checkplot1_nomc.pdf")
map(c(-1,0,1), function(ell){
    trycheckingobs %>% filter(l==ell) %>% 
        ggplot(aes(chaotile))+
        geom_histogram()+
        theme_classic()+
        facet_wrap(~size+l)
})
dev.off()

pdf(file="figures/do_richness_CI_ever_work.pdf")
trycheckingobs_R %>% 
  ggplot(aes(chaotile))+
  geom_histogram()+
  theme_classic()+
  labs(x="percentile of mean sample richness in 500k bootstrap samples", y="frequency")+
  facet_wrap(~size)
dev.off()

####################################
# set up data for users guide figures
nreps<-5000 #this should be 5000 for now

#df with true coverage of 95% CI
tvcov <-trycheckingobs_R %>% group_by(l, size) %>% summarize(outside=1-(sum(chaotile>97.5)+sum(chaotile<2.5))/500000)

#relable facets by creating new factor in df
inds<-data.frame("l"=c(1,0,-1), divind=factor(c("richness", "Hill-Shannon", "Hill-Simpson"), levels=c("richness", "Hill-Shannon", "Hill-Simpson")))

tc<-left_join(tvcov, inds)





###############################################
# confirm enough reps to get observed mean and true mean to be the same here
# checktruemu<-trycheckingobs %>% group_by(l, size) %>% summarize(tm=mean(truemu), om=mean(obsD))
# checktruemu %>% ggplot(aes(tm, om))+geom_point()
#################################################
# this is asymptotic diversity for users guide

#set reps to 500 but outerreps to 10 for efficient use of anotate
reps<-500
outerreps<-10

#####################
#uncomment to generate data for asymptotic diveristy checkplot/coverage for conceptual guide

# map(1:outerreps, function(x){
#   ug_asy<-map_dfr(round(10^seq(2, 4, 0.25)), function(size){
#       map_dfr(c(-1,0,1), function(l){
#           out<-checkplot(abs=usersguide, l=l, inds=size, reps=reps)
#       })
#   })
#   write.csv(ug_asy, paste("data/ug_asy",x, ".csv", sep="_"), row.names=F)
# })

#######################################
# extract data from files for use

outerreps<-10
getug<-map_dfr(1:outerreps, function(x){
    
    read.csv(paste("data/fromR/ug_asy", x, ".csv", sep="_"))
    
})

correct<-getug[seq(500, length(getug$l), by=500),]

asycov<-correct%>% 
    group_by(l, inds) %>% 
    summarize(outside=1-(sum(qtile>97.5)+sum(qtile<2.5))/(5000)) %>% left_join(inds)


###############################
#add a dataframe to get points where value="inf" with transformation, hack
otherdf<-data.frame(inds=round(10^seq(2, 4, 0.25)), outside=rep(1,9), divind=rep("richness",9), esttype=rep("sample diversity", 9))

# combine sample and saymptotic to make a single multi-panel graph 
comb_cov<-bind_rows("sample diversity"=tc %>% rename(inds=size), "asymptotic diversity"=asycov, .id="esttype" )
#code to generate plot
#to figure out y-axis breaks:  prettify(trans_breaks(arm::logit, invlogit, n=15)(c(0.5,0.9999999)))

pdf(file="figures/CI_coverage_guide.pdf", height=6, width=6) #
comb_cov %>% mutate(conserv=log(outside/(1-outside))) %>% 
    ggplot(aes(inds, outside, color=conserv, shape=esttype))+
    geom_point(size=2)+
    geom_point(data=otherdf, color="blue", size=2)+
    geom_hline(yintercept=0.95)+
    facet_grid(divind~esttype, switch="y" )+#strip.position=NULL
    theme_classic()+
    scale_shape_manual(values=c(17,15))+
    scale_color_gradient2(low="red",mid="grey", high="blue", limits=c(0,5), midpoint=2.944, breaks=c(0,5), labels=c(" over-confident",  " conservative"))+
    scale_y_continuous(trans="logit", limits=c(0.5, .999), breaks=c(0.5, 0.73, 0.88, 0.95, 0.98,0.99, .997,.999), labels=c(50, 73, 88, 95, 98, 99, 99.7, 99.9))+
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
##################################
# CODE FOR CHECKPLOT PAPER

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
  write.csv(tall, paste("data/fromR/sims",x, ".csv", sep="_"), row.names=F)
})
print(Sys.time()-start)



### get data from files



getdat2<-map_dfr(1:outerreps, function(x){

  read.csv(paste("data/fromR/sims", x, ".csv", sep="_"))

})

write.csv(getdat2, "data/fromR/bound_sims.csv", row.names=F)
map(1:outerreps, function(x){
  file.remove(list=paste("data/fromR/sims", x, ".csv", sep="_"))
})

map(1:84, function(x){
   file.remove(list=paste("data/fromR/sims", x, ".csv", sep="_"))
})


# read.csv("data/fromR/sims_1_.csv")

####################################
# data analysis

bound<- read.csv("data/fromR/bound_sims.csv")

brokelist<-lapply(c("com1", "com2", "com3"),function(comm){

       lapply(c(150,300,750), function(inds){
           lapply(c(-1,0,0.5,1), function(l){
               bound[which(bound$comm==comm& bound$inds==inds&bound$l==l),]
       })
   })

})


plts<-vector("list", 36)
a<-1

# for(x in c(1:3)){
#     for(y in c(1:3)){
#         for(z in c(1:4)){
#             plts[[a]]<-(brokelist[[x]][[y]][[z]] 
#                         %>% ggplot(aes(qtile))
#                         + geom_histogram()
#                         + labs(x="nominal p-value", y="frequency", title=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z])))
#             a<-a+1
#         }
#     }
# }
for(x in c(1:3)){

  pdf(file=paste("figures/",c("com1", "com2", "com3")[x], ".pdf", sep=""))
  par(mfrow=c(3,4))
  for(y in c(1:3)){
    for(z in c(1:4)){
      hist(brokelist[[x]][[y]][[z]]$qtile
           , xlab="nominal p-value", ylab="frequency", ylim=c(0,420000)
           , main=paste(c("com1", "com2", "com3")[x]
                        ,"inds=",c(150,300,750)[y]
                        , "l=", c(-1,0,0.5,1)[z]))
      abline(h=62500, col="red")
    }
  }
  dev.off()
}

for(x in c(1:3)){
  pdf(file=paste("figures/",c("com1", "com2", "com3")[x],"estimates", ".pdf", sep=""))
  par(mfrow=c(3,4))
  for(y in c(1:3)){
    for(z in c(1:4)){
      hist(brokelist[[x]][[y]][[z]]$chaoest, xlab="estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z]))
      abline(v=mean(brokelist[[x]][[y]][[z]]$truediv), col="red")
      abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
      abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")
      
    }
  }
  dev.off()
}

for(x in c(1:3)){
  pdf(file=paste("figures/",c("com1", "com2", "com3")[x],"estimates_standardized", ".pdf",sep=""))
  par(mfrow=c(3,4))
  for(y in c(1:3)){
    for(z in c(1:4)){
      hist(scale(brokelist[[x]][[y]][[z]]$chaoest), xlab="standardized \n estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z]), xlim=c(-3,10))
      abline(v=(mean(brokelist[[x]][[y]][[z]]$truediv)-mean(brokelist[[x]][[y]][[z]]$chaoest))/mean(brokelist[[x]][[y]][[z]]$chaoest), col="red")
      abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
      abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")
      
    }
  }
  dev.off()
  
   pdf(file=paste("figures/",c("com1", "com2", "com3")[x], ".pdf", sep=""))
    par(mfrow=c(3,4))
    for(y in c(1:3)){
        for(z in c(1:4)){
        hist(brokelist[[x]][[y]][[z]]$qtile
                            , xlab="nominal p-value", ylab="frequency", ylim=c(0,420000)
                            , main=paste(c("com1", "com2", "com3")[x]
                                         ,"inds=",c(150,300,750)[y]
                                         , "l=", c(-1,0,0.5,1)[z]))
        abline(h=62500, col="red")
        }
    }
    dev.off()
}

for(x in c(1:3)){
    pdf(file=paste("figures/",c("com1", "com2", "com3")[x],"estimates", ".pdf", sep=""))
    par(mfrow=c(3,4))
    for(y in c(1:3)){
        for(z in c(1:4)){
            hist(brokelist[[x]][[y]][[z]]$chaoest, xlab="estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z]))
            abline(v=mean(brokelist[[x]][[y]][[z]]$truediv), col="red")
            abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
            abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")

        }
    }
    dev.off()
}

for(x in c(1:3)){
    pdf(file=paste("figures/",c("com1", "com2", "com3")[x],"estimates_standardized", ".pdf",sep=""))
    par(mfrow=c(3,4))
    for(y in c(1:3)){
        for(z in c(1:4)){
            hist(scale(brokelist[[x]][[y]][[z]]$chaoest), xlab="standardized \n estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z]), xlim=c(-3,10))
            abline(v=(mean(brokelist[[x]][[y]][[z]]$truediv)-mean(brokelist[[x]][[y]][[z]]$chaoest))/mean(brokelist[[x]][[y]][[z]]$chaoest), col="red")
            abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
            abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")
            
        }
    }
    dev.off()
}

quantile(brokelist[[1]][[1]][[1]]$chaoest,0.025)


myhist<-function(comm, inds, l, var=c("chaoest", "standard", "checkplot")){
<<<<<<< HEAD
  x<-which(c("com1", "com2", "com3")==comm)
  y<-which(c(150,300,750)==inds)
  z<-which(c(-1,0,0.5,1)==l)
  if(var=="standard"){
    hist(scale(brokelist[[x]][[y]][[z]]$chaoest, scale=F)/mean(brokelist[[x]][[y]][[z]]$chaoest)
         , xlab=NULL
         , ylab=NULL
         , main=NULL
         , ylim=c(0,700000)
         , xlim=c(-0.7,2.2)
         #, xlab="standardized \n estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z])
    )
    abline(v=(mean(brokelist[[x]][[y]][[z]]$truediv)-mean(brokelist[[x]][[y]][[z]]$chaoest))/mean(brokelist[[x]][[y]][[z]]$chaoest), col="red")
    abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest, scale=F)/mean(brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
    abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest, scale=F)/mean(brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")
  }
  if(var=="chaoest"){
    hist(brokelist[[x]][[y]][[z]]$chaoest 
         , xlab=NULL
         , ylab=NULL
         , main=NULL
         , ylim=c(0,700000)
         #,xlab="estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z])
    )
    abline(v=mean(brokelist[[x]][[y]][[z]]$truediv), col="red")
    abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
    abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")
  }
  if(var=="checkplot"){
    hist(brokelist[[x]][[y]][[z]]$qtile
         , ylim=c(0,420000)
         , xlab=NULL
         , ylab=NULL
         , main=NULL
         # , xlab="nominal p-value", ylab="frequency"
         # , main=paste(c("com1", "com2", "com3")[x]
         #              ,"inds=",c(150,300,750)[y]
         #              , "l=", c(-1,0,0.5,1)[z])
    )
    abline(h=62500, col="red")
  }
  
=======
    x<-which(c("com1", "com2", "com3")==comm)
    y<-which(c(150,300,750)==inds)
    z<-which(c(-1,0,0.5,1)==l)
    if(var=="standard"){
        hist(scale(brokelist[[x]][[y]][[z]]$chaoest, scale=F)/mean(brokelist[[x]][[y]][[z]]$chaoest)
             , xlab=NULL
             , ylab=NULL
             , main=NULL
             , ylim=c(0,700000)
             , xlim=c(-0.7,2.2)
             #, xlab="standardized \n estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z])
             )
        abline(v=(mean(brokelist[[x]][[y]][[z]]$truediv)-mean(brokelist[[x]][[y]][[z]]$chaoest))/mean(brokelist[[x]][[y]][[z]]$chaoest), col="red")
        abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest, scale=F)/mean(brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
        abline(v=quantile(scale(brokelist[[x]][[y]][[z]]$chaoest, scale=F)/mean(brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")
    }
    if(var=="chaoest"){
        hist(brokelist[[x]][[y]][[z]]$chaoest 
             , xlab=NULL
             , ylab=NULL
             , main=NULL
             , ylim=c(0,700000)
             #,xlab="estimated diversity", ylab="frequency", main=paste(c("com1", "com2", "com3")[x],"inds=",c(150,300,750)[y], "l=", c(-1,0,0.5,1)[z])
             )
        abline(v=mean(brokelist[[x]][[y]][[z]]$truediv), col="red")
        abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.975), col="blue")
        abline(v=quantile((brokelist[[x]][[y]][[z]]$chaoest), 0.025), col="blue")
    }
    if(var=="checkplot"){
        hist(brokelist[[x]][[y]][[z]]$qtile
             , ylim=c(0,420000)
             , xlab=NULL
             , ylab=NULL
             , main=NULL
             # , xlab="nominal p-value", ylab="frequency"
             # , main=paste(c("com1", "com2", "com3")[x]
             #              ,"inds=",c(150,300,750)[y]
             #              , "l=", c(-1,0,0.5,1)[z])
             )
        abline(h=62500, col="red")
    }
    
>>>>>>> f76a6dd99964fd78e2d7b0db789f856b57dfdb78
}

pdf(width=6,height=3, file="figures/comm3_estimates_just_150_750.pdf")
par(mfrow=c(2,4), mar=c(2,2,2,0))
lapply(c(150, 750), function(inds){
<<<<<<< HEAD
  lapply(c(-1,0,0.5,1), function(l){
    options(scipen=5)
    myhist("com3",inds=inds,l=l, var="chaoest")
  })
=======
    lapply(c(-1,0,0.5,1), function(l){
        options(scipen=5)
        myhist("com3",inds=inds,l=l, var="chaoest")
    })
>>>>>>> f76a6dd99964fd78e2d7b0db789f856b57dfdb78
})
dev.off()

pdf(width=6,height=3, file="figures/comm3_standardized_just_150_750.pdf")
par(mfrow=c(2,4), mar=c(2,2,2,0))
lapply(c(150, 750), function(inds){
<<<<<<< HEAD
  lapply(c(-1,0,0.5,1), function(l){
    options(scipen=5)
    myhist("com3",inds=inds,l=l, var="standard")
  })
=======
    lapply(c(-1,0,0.5,1), function(l){
        options(scipen=5)
        myhist("com3",inds=inds,l=l, var="standard")
    })
>>>>>>> f76a6dd99964fd78e2d7b0db789f856b57dfdb78
})
dev.off()

pdf(width=6,height=3, file="figures/comm3_checkplots_just_150_750.pdf")
par(mfrow=c(2,4), mar=c(2,2,2,0))
lapply(c(150, 750), function(inds){
<<<<<<< HEAD
  lapply(c(-1,0,0.5,1), function(l){
    options(scipen=5)
    myhist("com3",inds=inds,l=l, var="checkplot")
  })
=======
    lapply(c(-1,0,0.5,1), function(l){
        options(scipen=5)
        myhist("com3",inds=inds,l=l, var="checkplot")
    })
>>>>>>> f76a6dd99964fd78e2d7b0db789f856b57dfdb78
})
dev.off()

#Chao et al. 2014 suggestion for CI on observed/rarefied Hill numbers is to use a normal approximation of the bootstrap. I haven't yet figured out how to check this but seems reasonable to begin by checking if normal makes any sense.

# obs<-replicate(5000,dfun(subsam(com3, 150), l=1))
# meanobs<-mean(obs)
# sigobs<-sd(obs)
# est<-rnorm(length(obs), mean=meanobs, sd=sigobs)
# ggable<-data.frame(obs=obs, est=est)
# 
# ggable %>% ggplot(aes(obs))+
#     geom_density(fill="blue", alpha=0.3)+
#     geom_density(aes(est), fill="red", alpha=0.3)+
#     theme_classic()

#yes, it is pretty much normal. 

#OK, Now write code to generate checkplot







# breakup<-map(c(150,300,750),function(inds){
#     bound %>% filter(inds==inds)
# })
# 
# brokeup<-map(breakup,function(subs){
#     map(c("com1", "com2", "com3"), function(comm){
#         subs %>% filter(comm==comm)
#     })
# })
# 
# rm(breakup)



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

########################
# look at fisher's alpha as sampling increases

plotalpha<-future_map_dfr(1:1000, function(x){
    map_dfr(10^seq(2, 4, 0.25), function(size){
        vec<-subsam(usersguide, size)
        vecnoz<-vec[which(vec>0)]
    alph<-fisherfit(vec)$estimate
    pl<-poilogMLE(vecnoz)
    logn<-fitdistr(vecnoz,densfun = "log-normal")
    return(data.frame(alph=alph, plm=pl$par[1], pls=pl$par[2], lnm=logn$estimate[1], lns=logn$estimate[2], size=size))
    })
})

plotalpha %>% ggplot(aes(size, lnm))+
    geom_point(alpha=0.02)+
    geom_point(aes(y=lns), color="red", alpha=0.02)+
    geom_point(aes(y=lns/lnm), color="blue", alpha=0.02)+
    geom_point(aes(y=pls), color="purple", alpha=0.02)
    theme_classic()+
    scale_x_log10()

plotalpha %>% ggplot(aes(size, exp(pls)))+geom_point(alpha=0.02)+theme_classic()+scale_x_log10()+geom_smooth(method="lm")
