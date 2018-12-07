library(tidyverse)
library(ggthemes)
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


rarity_plot <- function(abundance, p){
	rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	)
	div <- summarise(rf, div=ipfun(
		sum(abundance*pfun(rarity, p))/sum(abundance)
		, p
	)) %>% pull(div)
	rp <- (ggplot(rf, aes(x=rarity, y=abundance))
		+ geom_segment(aes(x=rarity, xend=rarity, y=abundance, yend=0))
		#could probably set breaks more flexibly
		+ scale_x_continuous(trans=power_trans(pow=p))
		#the expand=c(0,0) is what fixes x-axis in place at y=0
		+ scale_y_continuous(expand=c(0,0))
		#this is what makes the axis lines... it is just a line segment from min to max of the provided data. teh breaks are provided in scale_x_continuous or scale_y_continuous, wich in turn can get them from 
		+ geom_rangeframe(data=data.frame(rarity=c(min(rf$rarity), max(rf$rarity)), abundance=c(0,max(abundance)+10)))
		+ geom_vline(xintercept=div, color="red")
	)
	return(rp)
}

#ab <- c(20,30,50)
ab<-c(100, 20, 15, 10, 2, 1, 1,1)
rarity_plot(ab, 1)
rarity_plot(ab, 0)
rarity_plot(ab, -1)

