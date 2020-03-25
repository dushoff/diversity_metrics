# get loess predictions
library(data.table)
library(tidyverse)
library(mgcv)
library(furrr)

logit<-function(x){log(x/(1-x))}
invlogit<-function(x)(exp(x)/(1+exp(x)))

plan(strategy=multiprocess, workers=6)
bs<-fread("data/comm_samp_short.csv")

bs_short<-bs[,-c(1:200)] %>%
    gather(dtype, div, rich, shan, simp)

bs_sub <- bs_short %>% sample_n(5e3)


pdf("figures/loess_smooth_5e4_points.pdf")
bs_short %>% sample_n(5e3) %>% 
    ggplot(aes(log(tc/(1-tc)), div, color=SS))+
    geom_point(alpha=0.1)+
    facet_grid(dtype~comm, scales="free_y")+
    theme_classic()+
    geom_smooth(method="loess", formula=y~x, color="red")#, se=F)
dev.off()

combo<-bs_short %>% group_by(comm, dtype) %>% summarize(resi=n())

blist<-future_map(1:6, function(crow){
    assign(paste0("bs", crow), bs_sub %>% filter(comm==combo[crow, "comm"]& dtype==combo[crow, "dtype"]))
})

rm(bs_short)
loess_fun<- function(x){
    loess(div~tc, data=x)}

loess_res<-future_map(1:6, function(crow){
    my_x<-bs_sub %>% filter(comm==pull(combo[crow, "comm"])& dtype==pull(combo[crow, "dtype"]))
    loess_fun(my_x)
})

gam_fun<- function(x){
    gam(formula=div~s(tcl, bs="cs"), data=x)}

gam_res<-map(1:6, function(crow){
    my_x<-bs_short %>% filter(comm==pull(combo[crow, "comm"])& dtype==pull(combo[crow, "dtype"])) %>% mutate(tcl=logit(tc))
    my_x[my_x$tc==1, "tcl"]<-invlogit(7)  
    gam_fun(my_x)
})

newvals<-data.frame(tcl=seq(0.5,6.5,0.5))
get_predictions<-future_map_dfr(1:6, function(indx){
    ed<-predict.gam(gam_res[[indx]], newdata=newvals)
    data.frame(ed=ed
               , tc=invlogit(seq(0.5,6.5,0.5))
               , dtype=pull(combo[indx, "dtype"])
               , comm= pull(combo[indx, "comm"]))
    
    
})

get_predictions

fwrite(get_predictions, "data/gam_preds.csv")