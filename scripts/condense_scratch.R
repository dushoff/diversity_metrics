# file to read in checkplot data and make some CHECKPLOTS!
library(tidyverse)
# source("/Rstudio_Git/checkPlots/checkFuns.R")
library(furrr)
plan(strategy=multiprocess, workers=28)

##############
# to get new ASY data from scratch to fewer files. 
future_map(1:28, function(SAD){
    
    write.csv(map_dfr(1:1000, function(x){
        
        map_dfr(rev(round(10^seq(2, 5, 0.25))), function(size){
            map_dfr(c(-1,0,1), function(l){
                
                out<-tryCatch(read.csv(paste(
                    
                    "SAD", SAD, "_l_", l, "_inds_", size, "_outernew_",  x, "_.csv", sep="")
                    
                )
                , error=function(e){data.frame(c(rep(size,8),x))}
                )
                #I think this could be added to remove files as you go. 
                # tryCatch(file.remove(paste(
                # 
                #   "data/SAD", SAD, "_l_", l, "_inds_", size, "_outernew_",  x, "_.csv", sep="")
                # 
                # )
                # , error=function(e){
                #   print( paste(
                #   "data/SAD", SAD, "_l_", l, "_inds_", size
                # 
                #   , "_outernew_",  x, "_.csv   does not exist", sep=""))}
                # 
                # )
                
                return(data.frame(out))
            })
        })
        
    }))
    , paste0("data/asy_SAD", SAD, ".csv"), row.names=F)

}) 