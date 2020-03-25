# load libraries
library(data.table)
library(tidyverse)
library(iNEXT)
library(furrr)
library(tictoc)
# we have some kind of results to read in
tic()
csamples<-fread("data/new_samples_for_rarefaction.csv")
logit<-function(x){log(x/(1-x))}
invlogit<-function(x)(exp(x)/(1+exp(x)))
clev<-invlogit(seq(0.5, 5, 0.25))[
2
]
toc()
print("read")
plan(strategy=multiprocess, workers=24)
csamples<-csamples %>% mutate(rowind=1:nrow(csamples))
tic()
  one_level<-future_map_dfr(1:nrow(csamples), function(rown){
              data.frame(estimateD(
            as.numeric(csamples[rown, 1:200])
            , base = "coverage"
            , level = clev
            , conf = 0.95)
           %>% mutate(ell=1-order)
            , rowind=rown
        )
    })
    fwrite(one_level, file=paste0("data/coverage_rarefaction_at_",clev, ".csv"))
   print(paste0("wrote", clev))
toc()
