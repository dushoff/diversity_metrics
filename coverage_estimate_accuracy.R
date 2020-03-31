library(data.table)
library(tidyverse)
library(iNEXT)
dat<-fread("data/comm_samp_short.csv") 
ec_fun<-function(x){abus=as.numeric(x[1:200])
    iNEXT:::Chat.Ind(abus, sum(abus))}
ec_fun(c(dat[1,]))
logit<-function(x){log(x/(1-x))}
invlogit<-function(x)(exp(x)/(1+exp(x)))
ec<-apply(dat, 1, ec_fun)

with_ec<-dat %>% select(-c(1:200)) %>% bind_cols(ec=ec)

pdf("figures/coverage_estimates_logit_scale.pdf")
with_ec %>% filter(ec!=1, tc!=1) %>% 
    ggplot(aes(tc, ec))+ # color=SS
    geom_abline(slope=1)+
    geom_point(alpha=0.002)+
    facet_wrap(~comm, nrow=2)+
    theme_classic()+
    scale_x_continuous(trans="logit", breaks=c(round(invlogit(seq(-1, 8,1)),2), 0.995, 0.998, 0.999))+
    scale_y_continuous(trans="logit", breaks=c(round(invlogit(seq(-1, 8,1)),2), 0.995, 0.998, 0.999))+
    # scale_color_viridis_c()+
    coord_equal()+
    labs(x="true sample coverage", y="estimated sample coverage") #, color="sample size"
dev.off()

pdf("figures/coverage_estimates_arithmetic_scale.pdf")
with_ec %>% # filter(ec!=1, tc!=1) %>%
    ggplot(aes(tc, ec))+ # color=SS
    geom_abline(slope=1)+
    geom_point(alpha=0.002)+
    facet_wrap(~comm, nrow=2)+
    theme_classic()+
    # scale_x_continuous(trans="logit", breaks=c(round(invlogit(seq(-1, 8,1)),2), 0.995, 0.998, 0.999))+
    # scale_y_continuous(trans="logit", breaks=c(round(invlogit(seq(-1, 8,1)),2), 0.995, 0.998, 0.999))+
    # scale_color_viridis_c()+
    coord_equal()+
    labs(x="true sample coverage", y="estimated sample coverage") #, color="sample size"
dev.off()
