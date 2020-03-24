# get loess predictions
library(data.table)
library(tidyverse)
library(furrr)
plan(strategy=multiprocess, workers=6)
bs<-fread("data/comm_samp.csv")

bs_short<-bs[,-c(1:200)] %>%
    gather(dtype, div, rich, shan, simp)

rm(bs)

combo<-bs_short %>% group_by(comm, dtype) %>% summarize(resi=n())

blist<-future_map(1:6, function(crow){
    assign(paste0("bs", crow),bs_short %>% filter(comm==combo[crow, "comm"]& dtype==combo[crow, "dtype"]))
})

rm(bs_short)
loess_fun<- function(x){
    loess(div~tc, data=x)}

loess_res<-map(blist, function(obj){
    loess_fun(obj)
})
    


