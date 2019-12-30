###############
# file to read in checkplot data and make some CHECKPLOTS!
library(tidyverse)
source("/Rstudio_Git/checkPlots/checkFuns.R")
library(furrr)
plan(strategy=multiprocess, workers=7)


###########
# first look at the simple case. Read 1 .csv in and check it out. It looks like SAD 23 is the biggest file

Obs_SAD23<-read.csv("obs_SAD23.csv")
head(Obs_SAD23)


Obs_SAD6<-read.csv("obs_SAD6.csv")


Obs_SAD6 %>% group_by(l) %>% summarize(n())



read_bigs<-function(etype, x){read.csv(paste0(etype, "_SAD", x, ".csv"))}
byl<-function(x){x %>% group_by(l) %>% summarize(n())}

byl(read_bigs("asy", 2))

Obs_SAD23 %>% 
  mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>%
  #   # filter(hilld!="Hill-Shannon") %>%
  checkplot(facets=8)+
  theme_classic()+
  facet_grid(size~hilld, scales="free")+
  theme(panel.spacing.x = unit(2, "lines"))+
  scale_x_continuous(expand=c(0,0))


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
map(c(-1, 1), function(m){
        k<-mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>%
            filter(inds==100& l==m) %>%
            mutate(est=chaoest)



        rangePlot(k, target=mean(k$truediv)
                  , title=c("richness", "Hill-Shannon", "Hill-Simpson")[2-m], opacity=0.05)+
            theme_classic()


})

dev.off()

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
