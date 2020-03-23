# testing statistical coverage of coverage still to do
# 4)  for each SAD, bin observations, say 100 at a time, by true coverage. This means that in each bin you'd have 100 samples of very similar true coverage, they may have a variety of different sample sizes.
# 
# 5) For each bin, make a boxplot or similar for each Hill number. Goal is to visualize mean diversity given coverage, and the distribution. Should be clean-looking. Record mean diversity, mean coverage for each bin, hill number.
# 
# do similar to before but this time focus on chao estimated diversities

#     a) if target sample coverage is above or below true sample coverage for that sample
#     b) for each Hill diversity, if expected diversity for that coverage is in, below, or above Chao's interval
# c) sample stats like actual sample size, diversity, coverage
# 


# load libraries
library(data.table)
library(tidyverse)
library(iNEXT)
library(furrr)
library(tictoc)


# we have some kind of results to read in
tic()
csamples<-fread("data/new_samples_for_comparison.csv")
targets<-fread("data/mean_obs_cov.csv")
toc()
print("read")

# we also have some set of coverages we want, here's a placeholder in case I don't figure it out

# csamples<-baseline_samples from testing
csamples<-csamples %>% mutate(rowind=1:nrow(csamples))
tic()
the_chao_estimates<-future_map_dfr(1:nrow(csamples), function(rown){
    targs<-targets %>% fiilter(comm==csamples[rown, "comm"]) %>% pull(tc)
    map_dfr(targs, function(clev){
               data.frame(estimateD(
            as.numeric(csamples[rown, 1:200])
            , base = "coverage"
            , level = clev
            , conf = 0.95)
            %>% mutate(ell=1-order)
            , rowind=rown
        )
    })
})
toc()
print("crunched")

tic()
fwrite(the_chao_estimates, file="data/chaoests.csv")
toc()
print("wrote")
