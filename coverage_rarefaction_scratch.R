# time to be creative with the coverage data
library(furrr)
library(tidyverse)
#read data
bs<-fread("data/comm_samp.csv")

mc<-bs%>% group_by(comm, SS) %>% summarize_at(.vars=c("tc", "rich", "shan", "simp"), .funs=mean)
pdf
bs %>%
    gather(dtype, diversity, rich, simp, shan) %>%
    ggplot(aes(tc, diversity, color=comm))+
    geom_point()+
    facet_wrap(~dtype)+
    theme_classic()
