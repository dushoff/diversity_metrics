# script to make checkplots for Chao's variance estimators for diversity estimates. Also creates non-checkplot figure of CI coverage for Chao1 for the conceptual guide. 
library(tidyverse)
library(furrr)
source("scripts/helper_funs/estimation_funs.R")
library(mobsim)

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

pdf(height=2, width=6, file="figures/simssads.pdf")
par(mfrow=c(1,3))
plot(1:length(com1), com1, xlab="",  ylab="species abundance", type="line", ylim=c(0,26000))
plot(1:length(com2), com2, xlab="species rank", ylab="", type="line", ylim=c(0,26000))
plot(1:length(com3), com3, xlab="", ylab="", type="line", ylim=c(0,26000) )
dev.off()

asab<-function(namevec){as.numeric(table(namevec))}

dfun(com1, l=-1)
dfun(com2, l=-1)
dfun(com3, l=-1)




#gets slightly closer than obs
# show1<-checkplot(com1, l=0, inds=150, reps=1000)
# nc<-60#per Rob's recommendation
nc<-7

plan(strategy=multiprocess, workers=nc)

# make this cleaner
checkplot<-function(abs, B=2000, l, inds, reps){

  td<-dfun(abs, l)
  truemu_n<-mean(replicate(B,dfun(subsam(abs, inds),l)))
  future_map_dfr(1:reps,function(x){
    obs<-subsam(abs, size=inds)
    
    chaotile<-checkchao(obs, B, l, td, truemu_n=truemu_n)
    return(chaotile=data.frame(qtile=chaotile[1], mletile=chaotile[2], truediv=chaotile[3], trueme_n=chaotile[4], chaoest=chaotile[5], obsD=chaotile[6], l=rep(l, reps), inds=rep(inds, reps), reps=rep(reps, reps)))

  })
}


#make a fig for users guide

# ugcheck<-map_dfr(10^seq(1, 5.5, 0.5), function(inds){
#     checkplot(usersguide,B=1000, l=1, inds=inds, reps=1000)
# })

conceptual<-future_map_dfr(1:10000, function(x){
    map_dfr(round(10^seq(2, 4, 0.25)), function(size){
        subcom<-subsam(usersguide,size=size)
        annoyinglist<-Bootstrap.CI(subcom, q=0, datatype = "abundance")
        return(data.frame(size=size, lcl=annoyinglist["LCI.pro.2.5%"], ucl=annoyinglist["UCI.pro.97.5%"], est=Chao_Hill_abu(subcom, 0), obs=dfun(subcom, 1)))
    })
})

#write.csv(conceptual, "data/Chao1_example_for_guide.csv", row.names=F)
conceptual<-read.csv("data/Chao1_example_for_guide.csv")
w<-1

#### what is the maximum, since lines go way high but want to truncate y-axis at 200? 

## ok, now, what is the actual coverage of the true value with the sample sizes used
annotatedf<-conceptual %>% 
    mutate(ucl=est+ucl, lcl=est-lcl, mean=est) %>% 
    group_by(size) %>% summarize(coverage=length(which(ucl>=120))/100, uclucl=quantile(ucl, 0.975),maxucl=max(ucl))
labeldf<-data.frame(x="1000", y=5, lab="chance that estimator CI intersects true richness value (nominally 95%)")

annotatedf
pdf(width=8, height=6, file="figures/violins_for_guide.pdf")
# quartz(height=6,width=8)
textsize<-5
conceptual %>%
    mutate(ucl=est+ucl, lcl=est-lcl, mean=est) %>%
    gather(key="estimate", value="pointrange", mean, lcl, ucl) %>%
    mutate(estimate=factor(estimate, levels=c("ucl", "mean", "lcl"))) %>% 
    ggplot(aes(x=as.factor(size), y=pointrange, fill=estimate))+
    geom_hline(yintercept=120, color="red")+
    geom_violin(alpha=0.7, color="lightgrey", position="identity", width=1.4, size=0.2)+
    scale_fill_manual(values=c("blue","steelblue2", "lightseagreen"))+
    theme_classic()+
    ylim(c(0, 200))+
    geom_text(data=annotatedf, aes(x=as.factor(size), y=15, label=paste(round(coverage,1), "%", sep=""), color=coverage),fontface="bold", inherit.aes = F, size=textsize)+
    scale_color_gradient2(low="red", mid="maroon", high="slateblue", limits=c(41, 100),guide="none")+
    geom_text(aes(x=x, y=y, label=lab), data=labeldf, inherit.aes = F, size=textsize)+
    labs(x="individuals sampled", y="Chao1 estimated richness")+
    theme(text=element_text(size=14))


dev.off()

conceptual %>% 
    mutate(ucl=est+ucl, lcl=est-lcl, mean=est) %>% 
    group_by(size) %>% summarize(maxucl=max(ucl))





####### 
#get a checkplot for obs. 
comm<-usersguide

comm<-usersguide
size<-100
reps<-10
l<-0
truemu<-function(comm, size, reps, l,...){
    sam<-replicate(reps, subsam(comm, size))
    return(mean(apply(
        sam,2, function(x){dfun(x, l)}
    )
    )
    )
    }

truemu(usersguide, 100, 1000, -1)


reps<-1000



obscp<-function(l=l, size=size, dat=usersguide, B=2000, truemun=truemun...){
    sam<-subsam(usersguide, size)
    data.bt = rmultinom(B,size,Bt_prob_abu(sam))
    obs<-dfun(sam,l)
    pro = apply(data.bt,2,function(boot)Chao_Hill_abu(boot,1-l))
    pro<-pro-mean(pro)+obs
    chaotile<-sum(pro<=truemun)/(B/100)
    return(data.frame("chaotile"=chaotile, "truemu"=truemun,  "obsD"=obs, "l"=l, "size"=size ))
}

trycheckingobs<-map_dfr(round(10^seq(2, 4, 0.25)), function(size){
    map_dfr(c(-1,0,1), function(l){
        truemun<-truemu(usersguide, size=size, reps=reps, l=l)
          future_map_dfr(1:reps, function(reps){obscp(l, size, usersguide, truemun=truemun)})
    })
})
pdf(file="figures/empirical_checkplot1.pdf")
map(c(-1,0,1), function(ell){
    trycheckingobs %>% filter(l==ell) %>% 
        ggplot(aes(chaotile))+
        geom_histogram()+
        theme_classic()+
        facet_wrap(~size+l)
})

dev.off()
reps<-125

# str(checkchao(com1, 1000, 1, dfun(com1, 1)))
# checkplot(com1, 1000, 1, 150, 1)


checktruemu<-trycheckingobs %>% group_by(l, size) %>% summarize(tm=mean(truemu), om=mean(obsD))

View(checktruemu)

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

<<<<<<< HEAD

=======
    
>>>>>>> f76a6dd99964fd78e2d7b0db789f856b57dfdb78
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
<<<<<<< HEAD
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
=======
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
>>>>>>> f76a6dd99964fd78e2d7b0db789f856b57dfdb78
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
