library(tidyverse)
theme_set(theme_classic())

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
power_trans = function(pow) trans_new(name="power"
    , transform=function(x) pfun(x, pow, rever=T)
	, inverse=function(x) ipfun(x, pow, rever=T)
	#, breaks = function(x)extended_breaks(n = 30)(x)
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
		+ scale_x_continuous(trans=power_trans(pow=p), breaks=c(1.5,2,2.5,3,5,8))
		#+ scale_size_identity()
		+ geom_vline(xintercept=div, color="red")
	)
	return(rp)
}

ab <- c(20,30,50)

rarity_plot(ab, 1)
rarity_plot(ab, 0)
rarity_plot(ab, -1)



