library(tidyverse)
library(ggthemes)
library(cowplot)
theme_set(theme_tufte(base_family = "sans"))

require(scales) # trans_new() is in the scales library
#need the reverse bit for the scaling of the axes to get them in the right orientation
pfun=function(x, pow, rever=F){
	if (pow==0) return(log(x))
    if (rever==T){
    if (pow<0) return(-x^pow)
        return(x^pow)}
	return(x^pow)
}

ipfun=function(x, pow, rever=F){
	if (pow==0) return(exp(x))
    if(rever==T){if (pow<0) return(-x^(1/pow))
        return(x^(1/pow))}
	return(x^(1/pow))
}

#add a diversity function to make graphs more complicated
dfun<-function(ab, l){ipfun(sum(ab*pfun(sum(ab)/ab, l))/sum(ab),l)}

## This is weird craftiness to make functions above work with coord_trans

prettify <- function(breaks){
    # round numbers, more aggressively the larger they are
    digits <- -floor(log10(abs(breaks))) + 1
    digits[breaks == 0] <- 0
    return(round(breaks, digits = digits))
}


power_trans = function(pow) trans_new(name="power"
    , transform = function(x) pfun(x, pow, rever=T)
	, inverse = function(x) ipfun(x, pow, rever=T)
	, breaks = function(x) prettify(trans_breaks(function(x) pfun(x, pow, rever=T), function(x) ipfun(x, pow, rever=T), n =5) (c(0,x)*1.1))
    , domain = c(1, 10000)
)

fancy_rep<-function(df){data.frame(df[rep(1:nrow(df), df$abundance),])}

rarity_plot <- function(abundance, p){
	rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	)
	div <- summarise(rf, div=ipfun(
		sum(abundance*pfun(rarity, p))/sum(abundance)
		, p
	)) %>% pull(div)
	#I don't think all those letters are necessary at this point. 
	rf2<-data.frame("gr"=as.factor(1:nrow(fancy_rep(rf))),fancy_rep(rf))
	rp <- (ggplot(rf2, aes(x=rarity))
	       #This makes almost ok stacks of boxes. not ok because of scaling transformations
	       +geom_bar(aes(group=gr),fill="grey", colour="black", width=min(1/10*10^p, 0.05),size=0.1)
	       + geom_vline(xintercept=div, color="red", size=1.1)
	       # + geom_vline(xintercept=dfun(ab,1), color="blue", linetype=3, size=1)
	       # + geom_vline(xintercept=dfun(ab,-1), color="purple", linetype=4, size=1)
	       # + geom_vline(xintercept=dfun(ab,0), color="green", linetype=5, size=1)
	     #This makes a nice line that doesn't ahve issues with scaling
	# + geom_segment(aes(x=rarity, xend=rarity, y=abundance, yend=0), size=1.6)
		#could probably set breaks more flexibly
		+ scale_x_continuous(trans=power_trans(pow=p))
		#the expand=c(0,0) is what fixes x-axis in place at y=0
		+ scale_y_continuous(expand=c(0,0))

		#this is what makes the axis lines... it is just a line segment from min to max of the provided data. teh breaks are provided in scale_x_continuous or scale_y_continuous, wich in turn can get them from 
		+ geom_rangeframe(data=data.frame(rarity=c(min(rf$rarity), max(rf$rarity)), abundance=c(0,max(abundance)+10)))
	# + geom_point(aes(x=div, y=-2, color="red"))
	
	    + theme(legend.position="none")
		
	)
	return(rp)
}



ab <- c(20,30,50)
# ab<-c(100, 20, 15, 10, 2, 1, 1,1)
# ab<-c(50,20,30,5,3,2)

library(grid)


p<-function(x){ggdraw(x %>% add_sub(label="^", size=20, colour="blue", x=0.351, y=1.4, vjust=0))}


p(rarity_plot(ab,1))
p(rarity_plot(ab, 0))
p(rarity_plot(ab, -1))
