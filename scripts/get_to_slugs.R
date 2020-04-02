# slug up the coverage_rarefaction stuff

library(data.table)
library(tidyverse)
library(furrr)
plan(strategy=multiprocess, workers=7)
source("/Rstudio_Git/checkPlots/checkFuns.R")

logit<-function(x){log(x/(1-x))}
invlogit<-function(x)(exp(x)/(1+exp(x)))
#fucntion to select the closest value, to accomodate rounding etc. Hopefully there aren't really ambiguitities here anyways
find_targ<-function(x){tlist[which.min(abs(tlist-x))]}

#read in general data
sample_dat<-fread("data/new_samples_for_rarefaction_2.csv", drop=1:200) %>% 
    rowid_to_column(var="rowind") 
gam_preds<-fread("data/gam_preds_2.csv")

tlist<-invlogit(seq(0.5,5,0.25))

all_slug<-future_map_dfr(tlist, function(clev){
    tryCatch({
        #read rarefaction data one d.f. at a time
        newChaoests<-fread(paste0("data/coverage_rarefaction_at_", clev, "_2.csv"))
        # filter for only the "rarefied" results, at least for now.
        generous<-newChaoests %>% filter(method=="interpolated")
        rm(newChaoests)
        gc()
        #merge with original data for diversity statistics
        fuller<-generous %>% left_join(sample_dat, by="rowind") %>% 
            rowwise %>%  
            mutate(targ_cov=find_targ(SC))
    
        overfloweth<-fuller %>% left_join(
            gam_preds %>%
                mutate(ell=2-as.numeric(factor(gam_preds$dtype))) %>% 
                rowwise%>%mutate(targ_cov=find_targ(tc)) %>% 
                select(-c(tc, dtype)) %>% ungroup()
            , by=c("comm", "ell", "targ_cov")
        )
        
        first_slug<-overfloweth %>% rename(est=qD, lower=qD.LCL, upper=qD.UCL, p=rowind) %>% filter(method=="interpolated")
        
        rm(list=c("fuller", "overfloweth"))
        gc()
        
        try_slugs<-map_dfr(c("gamma_comm", "lnorm_comm"), function(C){
            map_dfr(c(-1,0,1), function(d){
                subslug=first_slug %>% 
                    filter(comm==C, ell==d) %>% ungroup()
                    map_dfr(seq(100,2000, 100), function(ss){
                        print(paste0("d = ", d, "; C= ", C, "; clevel = ", clev, "SS = ", ss))
                        mydf<-subslug %>% filter(SS==ss)
                        
                        data.frame(comm=C
                                    , clev=clev
                                    , ss=ss
                                    , ell=d
                                    , stat_cov= mydf %>% 
                                        summarize(stat_cov=100*(1-(sum(ed>upper)+sum(ed<lower))/n())
                                            ) 
                                    )
                       
                            # tryCatch(rangePlot(mydf, target=mean(mydf$ed), opacity=0.2
                            #                    , title=paste0("Hill diversity with ell = ", d
                            #                                   ,", ", C
                            #                                   , ", coverage = ", round(clev,3)
                            #                                   , ", SS = ", ss))
                            #     + labs(y="estimated Hill diversity")
                            #     , error=function(e){ggplot(data.frame(x=seq(0,1, 0.1), y=0:10),aes(x,y))+
                            #             labs(title=paste0("Hill diversity with ell = ", d
                            #                          ,", ", C
                            #                          , ", coverage = ", round(clev,3)
                            #                          , ", SS = ", ss))
                            #         }) #end tryCatch
                }) #loop over ss
            }) # loop over ell
        }) # loop over community
        return(try_slugs)
      
        }, error=function(e) data.frame(comm=NA, clev=0, ss=0, ell=1, stat_cov=NA))
    
})


pdf("figures/coverage_rarefaction_CI_too_narrow_2.pdf")
stat_cov_toplot<-all_slug%>% left_join(data.frame(ell=c(-1,0,1), hill=c("Hill-Simpson", "Hill-Shannon", "Richness"))) %>% 
  filter(!is.na(stat_cov)) 
stat_cov_toplot$hill<-factor(stat_cov_toplot$hill, levels=c("Hill-Simpson", "Hill-Shannon", "Richness"))
stat_cov_toplot%>% 
  ggplot(aes(clev, stat_cov, color=ss))+
  facet_grid(comm~hill)+
  geom_hline(yintercept=95, color="red")+
  geom_point(alpha=0.1)+
  theme_classic()+
  labs(y="% CI that overlap true expected diversity", x="sample coverage level", color="sample size")+ 
  theme(panel.spacing = unit(2, "lines"))

dev.off()

pdf("figures/coverage_rarefaction_slugplots_2.pdf")
try_slugs_snew
dev.off()

pdf("figures/coverage_rarefaction_slugs_1-per.pdf")
all_slug
dev.off()
# 
pdf("figures/coverage_rarefaction_slugs_faceted.pdf")
try_slugs_snew2
dev.off()

# pdf("figures/coverage_raref_and_extrap_faceted.pdf")
# try_slugs3
# dev.off()
