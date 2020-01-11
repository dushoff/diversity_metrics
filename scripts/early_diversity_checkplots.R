###############
# file to read in checkplot data and make some CHECKPLOTS!
library(tidyverse)
#to make checkplots, path only makes sense on my computer
source("/Rstudio_Git/checkPlots/checkFuns.R")
#to get info about each SAD
source("scripts/checkplot_initials.R")
library(furrr)
library(gridExtra)
library(grid)
plan(strategy=multiprocess, workers=7)


read_bigs<-function(etype, x){read.csv(paste0(etype, "_SAD", x, ".csv"))}
byl<-function(x){x %>% group_by(l) %>% summarize(n())}
pst_names<-function(x){paste(map(1:length(x)
                                 , function(y){
                                    paste0(names(x)[y]
                                      , ": "
                                      ,ifelse(is.na(as.numeric(x[y]))
                                                    , x[y]
                                                    , round(as.numeric(x[y]), 2)
                                                    )
                                              )})
                                   ,collapse = ", ")}

#sdlogs as a function
sdlogs<-function(x){
  x  %>% 
    gather(etype, div, chaoest, obsD ) %>% 
    mutate(div=as.numeric(div)) %>% 
    group_by(l, inds, SAD_ind, etype) %>% 
    summarize(sdlog=sd(log(div), na.rm=T), cv=sd(div, na.rm=T)/mean(div, na.rm=T))
}





###########
# going slow, figuring out how to make checkplot results look ok for all SADS, 
#possible results figures drafts
#will turn into a function if it works and make a pile of these.

##tester params
# SAD<-6
# ell<--1


pdf("figures/obs_cp.pdf", width=11, height=8.5)
future_map(1:24, function(SAD){
  map(c(-1,0,1), function(ell){
    SADinfo<-flatten(flatten(SADs_list))[[SAD]]
    read_bigs("data/obs", SAD) %>% 
      mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
      mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>%
      filter(l==ell) %>%
      checkplot(facets=15)+
      theme_classic()+
      facet_wrap(~size, scales="free", nrow=3, labeller=function(x){paste0("N = ", x)})+
      theme(panel.spacing.x = unit(1, "lines"), title=element_text(size=12))+
      scale_x_continuous(expand=c(0,0))+
      ggtitle(paste(paste0("sample "
                                   , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell]
                                   , " Checkplots")
                            ,"\n"
                            , pst_names(SADinfo$distribution_info)
                            ,"\n"
                            , pst_names(SADinfo$community_info) 
                            , collapse = ", "
                            , sep = " ")
              )
  })
})
dev.off()

pdf("figures/asy_cp.pdf", width=11, height=8.5)
future_map(1:24, function(SAD){
  myd<-read_bigs("data/asy", SAD) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson")))
  SADinfo<-flatten(flatten(SADs_list))[[SAD]]
  map(c(-1,0,1), function(ell){
    mydl<-myd %>% filter(l==ell)
    if(sum(mydl$p[!is.na(mydl$p)])>0){
       tryCatch(mydl %>% filter(!is.na(p))  %>%
        checkplot(facets=9)+
        theme_classic()+
        facet_wrap(~inds, scales="free", nrow=3)+
        theme(panel.spacing.x = unit(1, "lines"))+
        scale_x_continuous(expand=c(0,0))+
        ggtitle(paste(paste0("asymptotic "
                                              , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell]
                                              , " Checkplots")
                                       ,"\n"
                                       , pst_names(SADinfo$distribution_info)
                                       ,"\n"
                                       , pst_names(SADinfo$community_info) 
                                       , collapse = ", "
                                       , sep = " ")
                                 , gp=gpar(fontsize=12,font=3)))
        }
  })
})
dev.off()



  

#########################
# spend a few minutes on the very skewed SADS I have been playing with on my computer. 

############ make a few range plots???
pdf('figures/try_ASY_slugs_impoved_ti.pdf')
map(1:24, function(SAD){
  myd<-read_bigs("data/asy", SAD) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
    mutate(est=chaoest) %>% 
    filter(inds<3000)
  SADinfo<-flatten(flatten(SADs_list))[[SAD]]
  map(c(-1,0,1), function(ell){
    #store a bunch of slugs as a list
    slugs<- map(rev(unique(myd$inds[!is.na(myd$inds>0)])), function(inds){
      mydl<-myd %>% filter(l==ell, inds==inds)
      print(c(ell, SAD, inds))
        tryCatch(mydl %>%
                   rangePlot(title=paste0("sample N = ", inds), target=as.numeric(SADinfo$community_info[2-ell]))+
                   theme(title=element_text(size=7)),
                 error=function(e){ggplot(data.frame(x=seq(0,1, 0.1), y=0:10),aes(x,y))})
      
      
    })
    #plot all sample sizes for each ell
    grid.arrange(grobs=slugs, ncol=2
                          , top = textGrob(paste(paste0("asymptotic "
                                                                  , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell]
                                                                  , " slugplot")
                                                           ,"\n"
                                                           , pst_names(SADinfo$distribution_info)
                                                           ,"\n"
                                                           , pst_names(SADinfo$community_info) 
                                                           , collapse = ", "
                                                           , sep = " ")
                                                     , gp=gpar(fontsize=12,font=3)))
  })
})
dev.off()
# slugs for obs as well
pdf("figures/obs_SlugPlot.pdf", height=8.5, width=11)
map(1:24, function(SAD){
  SADinfo<-flatten(flatten(SADs_list))[[SAD]]
  bigdl<-read_bigs("data/obs", SAD) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
    mutate(est=obsD)
  map(c(-1,0,1), function(ell){
   
    slugs<- map(unique(bigdl$size[bigdl$size<3000]), function(inds){
      mydl<-bigdl %>% filter(l==ell, size==inds)
    mydl %>% filter(!is.na(p))  %>%
      rangePlot(title=paste0("sample N = ", inds))+
      theme(title=element_text(size=7))
  })
    grid.arrange(grobs=slugs, ncol=2, top = textGrob(paste(paste0("observed "
                                                                  , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell]
                                                                  , " slugplot")
                                                           ,"\n"
                                                           , pst_names(SADinfo$distribution_info)
                                                           ,"\n"
                                                           , pst_names(SADinfo$community_info) 
                                                           , collapse = ", "
                                                           , sep = " ")
                                                     , gp=gpar(fontsize=12,font=3)))
  })
})
dev.off()


pdf("figures/first_new_cps.pdf", height=4.5, width=8)

   # map(c(-1,0,1), function(l){
   map(rev(round(10^seq(2, 4, 0.25))), function(size){
   SAD %>% filter(size==size)  %>%
      mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
      #   mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>%
      #   # filter(hilld!="Hill-Shannon") %>%
        checkplot(facets=8)+
        theme_classic()+
        facet_grid(~l, scales="free")+
        theme(panel.spacing.x = unit(2, "lines"))+
        scale_x_continuous(expand=c(0,0))
    })
  })
})
dev.off()




##########
# code to look at actual sampling uncertainty

plan(strategy=multiprocess, workers=7)
getug<-future_map_dfr(1:24, function(SAD){
  x<-read.csv(paste0("data/asy_SAD", SAD, ".csv")) 
  x %>% bind_cols(SAD_ind=rep(SAD, length(x[,2])))
})


getug2<-future_map_dfr(1:24, function(SAD){
  x<-read.csv(paste0("data/asy_SAD", SAD, "_other.csv")) 
  x %>% bind_cols(SAD_ind=rep(SAD, length(x[,2])))
})

getobs<-future_map_dfr(1:24, function(SAD){
  x<-read.csv(paste0("data/obs_SAD", SAD, ".csv")) 
  x %>% bind_cols(SAD_ind=rep(SAD, length(x[,2])))
})

getnewSADs<-map_dfr(c(1,7,8,15,22), function(SAD){
  x<-read.csv(paste0("data/asy_SAD_special", SAD, ".csv")) 
  x<-x[!is.na(x[,2]),]
  x %>% bind_cols(SAD_ind=rep(SAD, length(x[,2])))
})

#summarize SDlog(diversity) # for some reason this seems to be taking a long time isn't THAT much data is it?
sdlogs<-getug2  %>% 
  gather(etype, div, chaoest, obsD ) %>% 
  group_by(l, inds, SAD_ind, etype) %>% 
  summarize(sdlog=sd(log(div), na.rm=T), cv=sd(div, na.rm=T)/mean(div, na.rm=T))


#just look at the two new SADs
# newsdlogs<-sdlogs(getnewSADs)

newsdlogs<-sdlogs(newest_SADS_from_annotate)

# #repeat for obs only
# sdlogs_O<-getobs  %>% 
#   group_by(l, size, SAD_ind) %>% 
#   summarize(sdlog=sd(log(obsD), na.rm=T), cv=sd(obsD, na.rm=T)/mean(obsD, na.rm=T))

###########
# some sad info useful for plotting
tomerge<-map_dfr(1:length(flatten(flatten(SADs_list))), function(SAD_ind){
  myinf<-flatten(flatten(SADs_list))[[SAD_ind]]
  data.frame(SAD_ind
             , dist = myinf$distribution_info[[1]]
             , rich = myinf$community_info[[1]]
             , even = round((myinf$community_info[3]-1)/(myinf$community_info[[1]]-1),2)
  )
})
#######
# make a mini figure to this effect that is a .png for google docs and just has a few

for(et in c("asymptotic estimator", "sample diversity")){
  et<-"asymptotic estimator"
  thekey<-str_split(et, pattern=" ")[[1]][1]
  print(thekey)
   
  dat<-newsdlogs %>% 
    left_join(tomerge) %>% 
    mutate(etype=factor(etype, labels=c("asymptotic estimator", "sample diversity"))) %>% 
    filter(as.numeric(l)<5) %>% 
    mutate(Hill_Diversity=c("richness", "Hill-Shannon", "Hill-Simpson")[2-as.numeric(l)]) %>%
    filter(etype==et) %>% 
    filter(even %in% c(0.05, 0.15, 0.5), rich==200)
  
  myp<-tryCatch({
    dat %>% ggplot(aes(as.numeric(inds), sdlog, color=Hill_Diversity, shape=Hill_Diversity))+
      geom_point()+
      geom_line()+
      facet_grid(dist+rich~even)+
      theme_classic()+scale_x_log10()+
      theme(axis.text.x=element_text(angle=90))+
      # geom_hline(yintercept=0.1)+
      
      labs(x="sample size", y=paste0("SD of log(", et, ") under random sampling"))
  })
  
  png(file=paste0("figures/variability_fig_for_maintext_"
                  , thekey
                  , ".png")
      ,height=756, width=1028, res=150)
  myp
  dev.off()
}



pdf(file="figures/sampling_variability_skewed.pdf", width=11, height=8.5)

 map(c("asymptotic estimator", "sample diversity"), function(et){
  dat<-newsdlogs %>% 
    left_join(tomerge) %>% 
    mutate(etype=factor(etype, labels=c("asymptotic estimator", "sample diversity"))) %>% 
    filter(as.numeric(l)<5) %>% 
    mutate(Hill_Diversity=c("richness", "Hill-Shannon", "Hill-Simpson")[2-as.numeric(l)]) %>%
    filter(etype==et) 
   
  tryCatch({
    dat %>% ggplot(aes(as.numeric(inds), sdlog, color=Hill_Diversity, shape=Hill_Diversity))+
      geom_point()+
      geom_line()+
      facet_grid(dist+rich~even)+
      theme_classic()+scale_x_log10()+
      theme(axis.text.x=element_text(angle=90))+
      # geom_hline(yintercept=0.1)+
     
    labs(x="sample size", y=paste0("SD of log(", et, ") under random sampling"))
    })
})
dev.off()


map(c(1,7,8,15,22), function(SAD){
  flatten(flatten(SADs_list))[[SAD]][1:2]
})



#look at SDlog of estimates
<<<<<<< HEAD
pdf("figures/ugly_variability_in_obsD.pdf")
sdlogs_O %>% ggplot(aes(size, sdlog, color=factor(l), shape=factor(l)))+
  geom_point()+
  geom_line()+
  facet_wrap(~SAD_ind, ncol=5)+
  theme_classic()+scale_x_log10()+
  theme(axis.text.x=element_text(angle=90))+
  # geom_hline(yintercept=0.1)+
  labs(x="sample size", y=paste0("SD of log(observed diversity) under random sampling"))

dev.off()
=======
  
  
  >>>>>>> a9bc6c38c1d2521ff2203fe88bda2821b1ae992a
