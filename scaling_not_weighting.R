# I want to play with visualizing the mean as the middle of a distribution, where different values of l correspond to different scalings of the axis. 
# future drafts could make the graph look like a see-saw, where the current vertical line is a fulcrum, and bars have units but no y-axis.

#start by taking a very simple distribution like 2,3,5
library(tidyverse)
library(vegan)

#use 20,30,50 so it's obvious why x!=3
a<-c(20,30,50)
#This script isn't that flexible yet b/c the x-axis ticks don't work for randomly generated data, for example
#a<-rpois(6,5)
a_norm<-a/sum(a) #normalized, rel abundances
a_rare<-1/a_norm #rarities

#make a d.f. for easy manipulation of scales in ggplot2
comm<-data.frame("rarity"=a_rare,"abundance"=a)


#need to figure out how to set bar widths the same, or better yet replace with boxes or somehting that looks like units (and then go back to 2,3,5 instead of 20,30,50)

#unweighted by # individuals (as far as I know this is not a diversity metric people use, although I've encountered mean and geometric mean rel. abundance)
comm %>% ggplot(aes(rarity, rep(1, length(a))))+
    geom_col(width=0.01)+
    geom_vline(xintercept=mean(a_rare), color="red")+
    theme_classic()

#weighted by # individuals

#Richness (arithmetic scale)
comm %>% ggplot(aes(rarity, abundance))+
    geom_col(width=0.01)+
    geom_vline(xintercept=renyi(comm$abundance, scales=0, hill=T), color="red")+
    theme_classic()

#rescale x axis
#shannon (log) scale
comm %>% ggplot(aes(rarity, abundance))+
    geom_col(width=0.01)+
    scale_x_continuous(trans="log", limits=c(1.8,6), breaks=c(2,3,4,5))+
    geom_vline(xintercept=renyi(comm$abundance, scales=1, hill=T), color="red")+
    theme_classic()

#Simpson (harmonic scale, see transformation (trans) for details)
comm %>% ggplot(aes(rarity, abundance))+
    scale_x_continuous(trans=scales::trans_new("recip", function(x){1/(-x)}, inverse=function(x){1/(-x)}))+
    geom_vline(xintercept=renyi(comm$abundance, scales=2, hill=T), color="red")+
    geom_col(width=0.01)+
    theme_classic()



