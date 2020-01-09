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
                                      ,ifelse(is.numeric(x[y]), round(x[y],2), x[y]))})
                                   ,collapse = ", ")}





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
      facet_wrap(~size, scales="free", nrow=3)+
      theme(panel.spacing.x = unit(1, "lines"))+
      scale_x_continuous(expand=c(0,0))+
      ggtitle(paste(paste0(
                     c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell], " Checkplot")

                    ,"\n"

                    , pst_names(SADinfo$distribution_info)
                    , pst_names(SADinfo$community_info) 
                    , collapse = ", "
                    , sep = ", "))
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
          , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell], " checkplot")
          ,"\n"
          , pst_names(SADinfo$distribution_info)
          , pst_names(SADinfo$community_info) 
          , collapse = ", "
          , sep = ", ")))
        }
  })
})
dev.off()



  

############ make a few range plots???
pdf('figures/try_ASY_slugs.pdf')
map(1:24, function(SAD){
  SAD<-3
  myd<-read_bigs("data/asy", SAD) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
    mutate(est=chaoest)
  SADinfo<-flatten(flatten(SADs_list))[[SAD]]
  map(c(-1,0,1), function(ell){
    #store a bunch of slugs as a list
   slugs<- map(unique(myd$inds), function(inds){
    mydl<-myd %>% filter(l==ell, inds==inds)
    if(sum(mydl$p[!is.na(mydl$p)])>0){
      tryCatch(mydl %>% filter(!is.na(p))  %>%
                 rangePlot(title=paste0("sample of ", inds, " individuals")))
      }
    })
   #plot all sample sizes for each ell
   grid.arrange(grobs=slugs, nrow=3, top = textGrob(paste(paste0("asymptotic "
                                                  , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell]
                                                  , " slugplot")
                                           ,"\n"
                                           , pst_names(SADinfo$distribution_info)
                                           ,"\n"
                                           , pst_names(SADinfo$community_info) 
                                           , collapse = ", "
                                           , sep = " ")
                                           , gp=gpar(fontsize=20,font=3)))
  })
})
dev.off()
#############################
# spend a few minutes on the very skewed SADS I have been playing with on my computer. 

pdf('figures/try_ASY_slugs_special.pdf')
map(c(7,15), function(x){
  #########
  # going to see what goes wrong here
  x<-15
  
  myd<-read.csv(paste0("data/asy_SAD_special", x, ".csv")) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
    mutate(est=chaoest)
  SADinfo<-flatten(flatten(SADs_list))[[x]]
  map(c(-1,0,1), function(ell){
    ell<-0
    mydl<-myd %>% filter(l==ell)
    if(sum(mydl$p[!is.na(mydl$p)])>0){
      tryCatch(mydl %>% filter(!is.na(p), inds<5000)  %>%
                 rangePlot(facet_num=unique(mydl$inds), orderFun=milli)+
                 facet_wrap(~inds, scales="free", nrow=3)+
                 theme(panel.spacing.x = unit(1, "lines"))+
                 ggtitle(paste(paste0("asymptotic "
                                      , c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell], " slugplot")
                               ,"\n"
                               , pst_names(SADinfo$distribution_info)
                               , pst_names(SADinfo$community_info) 
                               , collapse = ", "
                               , sep = ", ")))
    }
  })
})
dev.off()

# slugs for obs as well
pdf("figures/obs_SlugPlot.pdf", width=11, height=8.5)
future_map(1:24, function(SAD){
  bigdl<-read_bigs("data/obs", SAD) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
    mutate(est=obsD)
  yodl<-bigdl[seq(25, length(bigdl$p), 25),]
  map(c(-1,0,1), function(ell){
    SADinfo<-flatten(flatten(SADs_list))[[SAD]]
    mydl<-yodl%>%
      filter(l==ell) 
    mydl %>%
      # checkplot(facets=15)+
      rangePlot()+
      theme_classic()+
      facet_wrap(~size, scales="free", nrow=3)+
      theme(panel.spacing.x = unit(1, "lines"))+
      scale_x_continuous(expand=c(0,0), limits=c(0,1))+
      ggtitle(paste(paste0(
        c("richness", "Hill-Shannon", "Hill-Simpson")[2-ell], " slugplot")
        ,"\n"
        , pst_names(SADinfo$distribution_info)
        , pst_names(SADinfo$community_info) 
        , collapse = ", "
        , sep = ", "))
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


pdf("figures/first_new_cps.pdf", height=4.5, width=8)
map(1:24, function(SAD){
  my_obs_cps_sofar %>% # filter(!(inds %in% c(3162, 5623, 316,178, 1778, 31623, 56234, 17783, 10000))) %>%
    filter(SAD==SAD)  %>%
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>%
    # filter(hilld!="Hill-Shannon") %>%
    checkplot(facets=8)+
    theme_classic()+
    facet_grid(hilld~size, scales="free")+
    theme(panel.spacing.x = unit(2, "lines"))+
    scale_x_continuous(expand=c(0,0))
})
my_obs_cps_sofar %>% filter(!is.na(p)) %>%  summarize(n_distinct(SAD))
dev.off()
m<-1
pdf(file="figures/diversity_slugs_early.pdf", height=3.5, width=3.5)
<<<<<<< HEAD
map(c(-1,0, 1), function(m){
=======
map(c(-1, 1), function(m){
>>>>>>> a9bc6c38c1d2521ff2203fe88bda2821b1ae992a
        k<-mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>%
            filter(inds==100& l==m) %>%
            mutate(est=chaoest)



        rangePlot(k, target=mean(k$truediv)
                  , title=c("richness", "Hill-Shannon", "Hill-Simpson")[2-m], opacity=0.05)+
            theme_classic()


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

getnewSADs<-map_dfr(c(7,15), function(SAD){
  x<-read.csv(paste0("data/obs_SAD", SAD, ".csv")) 
  x %>% bind_cols(SAD_ind=rep(SAD, length(x[,2])))
})

#summarize SDlog(diversity) # for some reason this seems to be taking a long time isn't THAT much data is it?
sdlogs<-getug2  %>% 
  gather(etype, div, chaoest, obsD ) %>% 
  group_by(l, inds, SAD_ind, etype) %>% 
  summarize(sdlog=sd(log(div), na.rm=T), cv=sd(div, na.rm=T)/mean(div, na.rm=T))

sdlogs<-function(x){
  x  %>% 
  gather(etype, div, chaoest, obsD ) %>% 
  group_by(l, inds, SAD_ind, etype) %>% 
  summarize(sdlog=sd(log(div), na.rm=T), cv=sd(div, na.rm=T)/mean(div, na.rm=T))
}
newsdlogs<-sdlogs(getnewSADs)


#repeat for obs only
sdlogs_O<-getobs  %>% 
  group_by(l, size, SAD_ind) %>% 
  summarize(sdlog=sd(log(obsD), na.rm=T), cv=sd(obsD, na.rm=T)/mean(obsD, na.rm=T))


pdf(file="figures/sampling_variability_2.pdf")

  # pdf(file="figures/sampling_variabilit.pdf")

# future_map(c(1:4, 5:8, 9:12, 13:16, 17:20, 21:24), function(x){SAD_ind %in%x &
map(c("chaoest", "obsD"), function(et){
  dat<-sdlogs %>%
    filter(etype==et)
  tryCatch({
    dat %>% ggplot(aes(inds, sdlog, color=factor(l), shape=factor(l)))+
      geom_point()+
      geom_line()+
      facet_wrap(~SAD_ind, ncol=5)+
      theme_classic()+scale_x_log10()+
      theme(axis.text.x=element_text(angle=90))+
      # geom_hline(yintercept=0.1)+
     
    labs(x="sample size", y=paste0("SD of log(", et, ") under random sampling"))
    })
})
dev.off()
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
