###############
# file to read in checkplot data and make some CHECKPLOTS!
library(tidyverse)
# source("/Rstudio_Git/checkPlots/checkFuns.R")
library(furrr)
plan(strategy=multiprocess, workers=7)


one_obs<-read.csv("data/new_trycheckingobs_SAD_10iter_1.csv")


byl(one_obs)

#next step is to repeat this but sending the newer .csvs to the data directory, then will bind rows from the ones in the home directory.

map(1:24, function(SAD){
  write.csv(bind_rows(
    read.csv(paste0("data/asy_SAD", SAD,  ".csv"))
             , future_map_dfr(1:1000, function(x){
        map_dfr(rev(round(10^seq(2, 4, 0.25))), function(size){
          map_dfr(c(-1,0,1), function(l){
                
                out<-tryCatch(read.csv(paste(
                    "data/SAD", SAD, "_l_", l, "_inds_", size, "_outernew_",  x, "_.csv", sep="")
                    )
                    , error=function(e){data.frame(c(rep(size,8),x))}
                  )
                tryCatch(file.remove(paste(
                  "data/SAD", SAD, "_l_", l, "_inds_", size, "_outernew_",  x, "_.csv", sep="")
                )
                , error=function(e){
                  print( paste(
                  "data/SAD", SAD, "_l_", l, "_inds_", size
                  , "_outernew_",  x, "_.csv   does not exist", sep=""))}
                )
          
                return(data.frame(out))
          })
      })
  }))
  , paste0("data/asy_SAD", SAD, ".csv"), row.names=F)
})     
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
)
  
  
  
  
  
  

#thsi combine: try bind rows of the old and the new. If it works sesms easy to recycles in the loop
future_map(1:24, function(SAD){
  tryCatch(expr={
    curr<-read.csv(paste0("data/asy_SAD", SAD, ".csv"))
    toadd<-read.csv(paste0("asy_SAD", SAD, ".csv"))
    out<-bind_rows(curr, toadd)
    write.csv(out, paste0("data/asy_SAD", SAD, ".csv"))
    file.remove(paste0("asy_SAD", SAD, ".csv"))
  }
  )
})

############## this is kind of nice there was no size on the new trycheckingobs ones so I think I can try this again and get it right. #######

map(1:24, function(SAD){
    write.csv(
      future_map_dfr(1:100, function(x){
        # map_dfr(rev(round(10^seq(2, 4, 0.25))), function(size){
        
          out<-tryCatch(read.csv(paste(
            "data/new_trycheckingobs_SAD_", SAD,  "iter_",  x,".csv", sep="")
            )
          , error=function(e){data.frame(c(rep(SAD,9),x))}
          )
          
          print(c(SAD, x))
          
          out<-out[!is.na(out$p),]
          
          tryCatch(file.remove(paste(
            "data/new_trycheckingobs_SAD_", SAD,  "iter_",  x, ".csv", sep="")
            )
          , error=function(e){print(paste(
              "data/new_trycheckingobs_SAD_", SAD,  "iter_",  x, ".csv", sep="")
              , "does not exist")
            }
          )
          
          return(data.frame(out))
      })
  # })
    , paste0("data/obs_SAD", SAD, ".csv"), row.names=F)
  })      



# 
# obs<-future_map(my_obs_cps_sofar, function(rep){
#   map(rep, function(SAD){
#     SAD<-SAD[!is.na(SAD$p),]
#   })
# })
#     
# 
#   
# pdf("figures/first_new_cps.pdf", height=4.5, width=8)
# future_map(obs, function(rep){
#  map(rep, function(SAD){
#    # map(c(-1,0,1), function(l){
#    map(rev(round(10^seq(2, 4, 0.25))), function(size){
#    SAD %>% filter(size==size)  %>% 
#       mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>%
#       #   mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>%
#       #   # filter(hilld!="Hill-Shannon") %>%
#         checkplot(facets=8)+
#         theme_classic()+
#         facet_grid(~l, scales="free")+
#         theme(panel.spacing.x = unit(2, "lines"))+
#         scale_x_continuous(expand=c(0,0))
#     })
#   })
# })
# dev.off()
# 
# 
# pdf("figures/first_new_cps.pdf", height=4.5, width=8)
# map(1:24, function(SAD){
#   my_obs_cps_sofar %>% # filter(!(inds %in% c(3162, 5623, 316,178, 1778, 31623, 56234, 17783, 10000))) %>%
#     filter(SAD==SAD)  %>% 
#     mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>% 
#     mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
#     # filter(hilld!="Hill-Shannon") %>% 
#     checkplot(facets=8)+
#     theme_classic()+
#     facet_grid(hilld~size, scales="free")+
#     theme(panel.spacing.x = unit(2, "lines"))+
#     scale_x_continuous(expand=c(0,0))
# })
# my_obs_cps_sofar %>% filter(!is.na(p)) %>%  summarize(n_distinct(SAD)) 
# dev.off()
# m<-1
# pdf(file="figures/diversity_slugs_early.pdf", height=3.5, width=3.5)
# map(c(-1, 1), function(m){
#         k<-mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>% 
#             filter(inds==100& l==m) %>% 
#             mutate(est=chaoest) 
# 
#         
#         
#         rangePlot(k, target=mean(k$truediv)
#                   , title=c("richness", "Hill-Shannon", "Hill-Simpson")[2-m], opacity=0.05)+
#             theme_classic()
#         
#          
# })
# 
# dev.off()
# 
# pdf(file="figures/diversity_slugs_early_Simp_10000.pdf", height=3.5, width=3.5)
# rangePlot(mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>% 
#     filter(inds==10000& l==-1) %>% 
#     mutate(est=chaoest) ,
#     target=10
#     , title=c("richness", "Hill-Shannon", "Hill-Simpson")[3], opacity=0.05)+
#     theme_classic()
# dev.off()
# 
# pdf(file="figures/diversity_slugs_early_RICH_10000.pdf", height=3.5, width=3.5)
# rangePlot(mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>% 
#               filter(inds==10000& l==1) %>% 
#               mutate(est=chaoest) ,
#           target=200
#           , title=c("richness", "Hill-Shannon", "Hill-Simpson")[1], opacity=0.05)+
#     theme_classic()
# dev.off()
