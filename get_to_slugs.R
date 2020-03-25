# slug up the coverage_rarefaction stuff

library(data.table)
library(tidyverse)
library(furrr)
plan(strategy=multiprocess, workers=7)
source("/Rstudio_Git/checkPlots/checkFuns.R")

logit<-function(x){log(x/(1-x))}
invlogit<-function(x)(exp(x)/(1+exp(x)))

# get some data
chaoests<-fread("data/chaoests_full.csv")
sample_dat<-fread("data/new_samples_for_comparison.csv")
gam_preds<-fread("data/gam_preds.csv")
# head(sample_dat)
generous<-chaoests %>% filter(method=="interpolated")

fullest<-generous %>% 
    left_join(sample_dat %>% 
                  rowid_to_column(var="rowind") %>% 
                  select(-c(2:201))
              , by="rowind")

find_targ<-function(x){tlist[which.min(abs(tlist-x))]}

tlist<-invlogit(seq(0.5, 6.5, 0.5))

fuller<-fullest %>% rowwise %>%  
    mutate(targ_cov=find_targ(SC))

overfloweth<-fuller %>% left_join(
    gam_preds %>%
        mutate(ell=2-as.numeric(factor(gam_preds$dtype))) %>% 
        rowwise%>%mutate(targ_cov=find_targ(tc)) %>% 
        select(-c(tc, dtype)) %>% ungroup()
    , by=c("comm", "ell", "targ_cov")
)
first_slug<-overfloweth %>% rename(est=qD, lower=qD.LCL, upper=qD.UCL, p=rowind) %>% filter(method=="interpolated")

try_slugs<-map(c("gamma_comm", "lnorm_comm"), function(C){
    map(c(-1,0,1), function(d){
        future_map(invlogit(seq(0.5,6.5,0.5)), function(clevel){
            print(paste0("d = ", d, "; C= ", C, "; clevel = ", clevel))
            mydf<-first_slug %>% 
                filter(comm==C, ell==d, targ_cov==clevel) %>% ungroup() 
                tryCatch(rangePlot(mydf, target=mean(mydf$ed), opacity=0.2
                                   , title=paste0("Hill diversity with ell = ", d,", ", C, ", coverage =", round(clevel,3)))
                    + labs(y="estimated Hill diversity")
                    # + facet_wrap(~SS)
                    , error=function(e){ggplot(data.frame(x=seq(0,1, 0.1), y=0:10),aes(x,y))})
        })
    })
})

pdf("figures/coverage_rarefaction_slugplots.pdf")
try_slugs
dev.off()
# 
pdf("figures/coverage_rarefaction_slugs_faceted.pdf")
try_slugs2
dev.off()

# pdf("figures/coverage_raref_and_extrap_faceted.pdf")
# try_slugs3
# dev.off()
