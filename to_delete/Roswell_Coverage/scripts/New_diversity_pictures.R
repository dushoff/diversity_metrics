#### This is a script to explore graphical explanations for the intuitive need for a generalized (specifically, one driven by smaller numbers than arithmetic) mean for describing the center of a skewed distribution. 

#Load general packages always used
require(vegan)
require(reshape2)        
require(ggplot2)
require(dplyr)
require(scales)


#play with lognormal distributions 

rich <- 5
quantile_vect <- seq(0,1,length.out = (rich +2))
quantile_vect <- quantile_vect[2:(rich+1)]; #this will make
#evenly spaced draws from the lognormal species abundance
#distribution
sku<-2
abundances <- qlnorm(quantile_vect, sdlog=sku)
ab<-data.frame(abundances/sum(abundances))
names(ab)<-"ab"
relab<- arrange(ab,desc(ab))
relab
named<-data.frame(cbind(seq(1:rich), relab))
names(named)<-c( "species","rel_abund")
head(named)
ggplot(named,aes(species,rel_abund))+geom_bar(stat="identity", color="black",alpha=(named$rel_abund/max(named$rel_abund)))+theme_classic()
