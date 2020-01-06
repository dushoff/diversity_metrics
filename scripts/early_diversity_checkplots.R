###############
# file to read in checkplot data and make some CHECKPLOTS!
library(tidyverse)
#to make checkplots, path only makes sense on my computer
source("/Rstudio_Git/checkPlots/checkFuns.R")
#to get info about each SAD
source("scripts/checkplot_initials.R")
library(furrr)
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
<<<<<<< HEAD
                    ,"\n"
=======
>>>>>>> a9bc6c38c1d2521ff2203fe88bda2821b1ae992a
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
  
<<<<<<< HEAD
############ make a few range plots???
pdf('figures/try_ASY_slugs.pdf')
future_map(1:24, function(SAD){
  myd<-read_bigs("data/asy", SAD) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
    mutate(est=chaoest)
  SADinfo<-flatten(flatten(SADs_list))[[SAD]]
  map(c(-1,0,1), function(ell){
    mydl<-myd %>% filter(l==ell)
    if(sum(mydl$p[!is.na(mydl$p)])>0){
      tryCatch(mydl[seq(25, length(mydl$p), 25),] %>% filter(!is.na(p))  %>%
                 rangePlot()+
                 theme_classic()+
                 facet_wrap(~inds, scales="free", nrow=3)+
                 theme(panel.spacing.x = unit(1, "lines"))+
                 scale_x_continuous(expand=c(0,0) limits=c(0,1))+
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
=======

>>>>>>> a9bc6c38c1d2521ff2203fe88bda2821b1ae992a


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

<<<<<<< HEAD

#figure out why some slugplots are funny looking, save changes
myp<-mydl %>% filter(size==1778, l==0) %>% mutate(est=obsD)

myp[seq(25, length(myp$p), 25),] %>% rangePlot()

=======
>>>>>>> a9bc6c38c1d2521ff2203fe88bda2821b1ae992a
pdf(file="figures/diversity_slugs_early_Simp_10000.pdf", height=3.5, width=3.5)
rangePlot(mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>%
    filter(inds==10000& l==-1) %>%
    mutate(est=chaoest) ,
    target=10
    , title=c("richness", "Hill-Shannon", "Hill-Simpson")[3], opacity=0.05)+
    theme_classic()
dev.off()

pdf(file="figures/diversity_slugs_early_RICH_10000.pdf", height=3.5, width=3.5)
rangePlot(mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>%
              filter(inds==10000& l==1) %>%
              mutate(est=chaoest) ,
          target=200
          , title=c("richness", "Hill-Shannon", "Hill-Simpson")[1], opacity=0.05)+
    theme_classic()
dev.off()
