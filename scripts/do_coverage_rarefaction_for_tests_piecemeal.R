# testing statistical coverage of coverage still to do
# 4)  for each SAD, bin observations, say 100 at a time, by true coverage. This means that in each bin you'd have 100 samples of very similar true coverage, they may have a variety of different sample sizes.
# 
# 5) For each bin, make a boxplot or similar for each Hill number. Goal is to visualize mean diversity given coverage, and the distribution. Should be clean-looking. Record mean diversity, mean coverage for each bin, hill number.
# 
# do similar to before but this time focus on chao estimated diversities

#     a) if target sample coverage is above or below true sample coverage for that sample
#     b) for each Hill diversity, if expected diversity for that coverage is in, below, or above Chao's interval
# c) sample stats like actual sample size, diversity, coverage
library(tidyverse)
map(c(1:13), function(tar){
    
    mycode<-c(
        "# load libraries"
        ,"library(data.table)"
        ,"library(tidyverse)"
        ,"library(iNEXT)"
        ,"library(furrr)"
        ,"library(tictoc)"
        
        ,"# we have some kind of results to read in"
        ,"tic()"
        ,"csamples<-fread(\"data/new_samples_for_rarefaction.csv\")"
        
        ,"logit<-function(x){log(x/(1-x))}"
        ,"invlogit<-function(x)(exp(x)/(1+exp(x)))"
        
        , "clev<-invlogit(seq(0.5, 5, 0.25))["
        , tar, "]"
        , "toc()"
        , "print(\"read\")"
        
        , "plan(strategy=multiprocess, workers=24)"
        , "csamples<-csamples %>% mutate(rowind=1:nrow(csamples))"
        
        
        , "tic()"
    
        , "  one_level<-future_map_dfr(1:nrow(csamples), function(rown){"
        , "              data.frame(estimateD("
        , "            as.numeric(csamples[rown, 1:200])"
        , "            , base = \"coverage\""
        , "            , level = clev"
        , "            , conf = 0.95)"
        , "           %>% mutate(ell=1-order)"
        ,"            , rowind=rown"
        , "        )"
        , "    })"
        , "    fwrite(one_level, file=paste0(\"data/coverage_rarefaction_at_\",clev, \".csv\"))"
        , "   print(paste0(\"wrote\", clev))"
        
        , "toc()"
    )
    
    write_lines(mycode, paste0("scripts/raref_for_test_", tar-1, ".R"))
})





