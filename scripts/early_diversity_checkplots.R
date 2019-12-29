###############
# file to read in checkplot data and make some CHECKPLOTS!
library(tidyverse)
source("/Rstudio_Git/checkPlots/checkFuns.R")
mycps_sofar<-map_dfr(1:1000, function(x){
      for(SAD in 1:24){
        paste("sad",SAD, sep="")<-map_dfr(rev(round(10^seq(2, 4, 0.25))), function(size){
          map_dfr(c(-1,0,1), function(l){
                
                out<-tryCatch(read.csv(paste(
                    "data/SAD", SAD, "_l_", l, "_inds_", size, "_outer_",  x, "_.csv", sep="")
                    )
                    , error=function(e){data.frame(c(rep(size,8),x))}
                  )
                print(c(SAD, size, l))
                return(data.frame(out, SAD=SAD))
          })
        }
      })
})      


my_obs_cps_sofar<-map(1:100, function(x){
  map(1:24, function(SAD){
    map_dfr(rev(round(10^seq(2, 4, 0.25))), function(size){
        
        out<-tryCatch(read.csv(paste(
          "data/new_trycheckingobs_SAD_", SAD,  "iter_",  x,"size", size, ".csv", sep="")
        )
        , error=function(e){c(rep(size,9),x)}
        )
        print(c(SAD, size, x))
        return(data.frame(out))
    })
  })
})      

my_obs_cps_sofar<-my_obs_cps_sofar[!is.na(my_obs_cps_sofar$p),]

my_obs_cps_sofar %>% group_by(SAD, l, size) %>% 
  summarize(good=n()) %>% 
  filter(good<5e4) %>% 
  arrange(good)

my_obs_cps_sofar<-map_dfr(1:100, function(x){
  map_dfr(1:24, function(SAD){
    map_dfr(rev(round(10^seq(2, 4, 0.25))), function(size){
      
      out<-tryCatch(read.csv(paste(
        "data/new_trycheckingobs_SAD_", SAD,  "iter_",  x,"size", size, ".csv", sep="")
      )
      , error=function(e){c(rep(size,9),x)}
      )
      print(c(SAD, size, x))
      return(data.frame(out, SAD=SAD))
    })
  })
})      


mycps_sofar %>% filter(l!=0) %>% group_by(SAD, inds, l) %>% summarize(succeed=n()) %>% arrange((succeed))

View(my_obs_cps_sofar %>%  group_by(SAD, size, l) %>% summarize(succeed=n()) %>% arrange((succeed)))

e2<-read.csv("data/new_trycheckingobs_SAD_10iter_100size100.csv")

e2
examp<-read.csv("data/SAD15_l_-1_inds_100_outer_1_.csv")
library(furrr)
plan(strategy=multiprocess, workers=7)
obs<-future_map(my_obs_cps_sofar, function(rep){
  map(rep, function(SAD){
    SAD<-SAD[!is.na(SAD$p),]
  })
})
    

  
pdf("figures/first_new_cps.pdf", height=4.5, width=8)
future_map(my_obs_cps_sofar, function(rep){
 map(rep, function(SAD){
   # map(c(-1,0,1), function(l){
   map(rev(round(10^seq(2, 4, 0.25))), function(size){
   SAD %>% filter(size==size)  %>% 
      # mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
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
