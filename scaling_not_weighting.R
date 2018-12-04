# I want to play with visualizing the mean as the middle of a distribution, where different values of l correspond to different scalings. 

#start by taking a very simple distribution like 2,3,5
library(tidyverse)
library(vegan)

a<-c(2,3,5)
a_norm<-a/sum(a)
a_rare<-1/a_norm
a_rare
#unweighted by # individuals
heights<-rep(1, length(a_rare))
plot(a_rare,heights, type="h", )
abline(v=mean(a_rare), col="red")

#weighted by # individuals
plot(a_rare,a, type="h", ylim=c(0,6) )
abline(v=1/(mean(a)/sum(a)), col="red")

#rescale x axis
plot(a_rare,a, type="h", ylim=c(0,6), log="x" )
abline(v=mean(a_rare), col="red")



