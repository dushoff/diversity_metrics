# plot the coverage coverage test

# 7) Find a fun way to plot the upshot: whether statistical coverage of 95%, whether misses are 2.5% high and 2.5% low, how this depends on sample size, coverage, and Hill number.
# 
# 8) Bonus, but might want to do this earlier and with a simple small dataset: biplot of true coverage and Chao's estimated coverage. Lost track of data from the other day and so going to backburner this for now and get all of this up and running on big computer asap.
# 
# bs is baseline_samples
#mc is summarized by mean value for each sample size.
 fun_fun<-function(x){ecdf(bs[bs$comm==x["comm"], bs$tc])(x["tc"])}
mc %>% mutate(cov_quant=apply(mc, MARGIN=1, fun_fun))
 
mc<-mc%>% mutate(cov_rank=cov_quant*1999999+0.5)


truth_1<-
    map_dfr(c("gamma_comm", "lnorm_comm"),function(SAD){ 
        map_dfr(mc %>%filter(comm==SAD) %>% pull (cov_rank), function(ctarg){ 
            dfout=bs_rank %>% 
                filter(comm==SAD & abs(c_rank-ctarg)<51) 
            return(dfout %>% 
                       group_by(comm) %>% 
                       summarize_at(.vars=c("SS", "tc", "rich", "shan", "simp"), .funs=mean))
            })
        })
    
                    
# baseline_samples %>% 
#     gather(dtype, diversity, rich, simp, shan) %>% 
#     ggplot(aes(tc, diversity, color=comm))+
#     geom_point()+
#     facet_wrap(~dtype)+
#     theme_classic()