library(tidyverse)
theme_set(theme_bw())

require(scales) # trans_new() is in the scales library

pfun=function(x, pow){
	if (pow==0) return(log(x))
	return(x^pow)
}
ipfun=function(x, pow){
	if (pow==0) return(exp(x))
	return(x^(1/pow))
}

## This is weird craftiness to make functions above work with coord_trans
power_trans = function(pow) trans_new(name="power"
	, transform=function(x) pfun(x, pow)
	, inverse=function(x) ipfun(x, pow)
	, breaks = function(x)extended_breaks(n = 30)(x)
)

## pow does not work if passed, but does work if set globally
rarity_plot <- function(abundance, p){
	rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	)
	div <- summarise(rf, div=ipfun(
		sum(abundance*pfun(rarity, p))/sum(abundance)
		, p
	)) %>% pull(div)
	rp <- (ggplot(rf, aes(x=names, y=rarity, size=abundance))
		+ geom_point() + scale_size_area()
		+ coord_trans(y=power_trans(pow=p))
		+ scale_y_continuous(breaks=c(1, 2, 5, 10, 20, 30))
		+ geom_hline(yintercept=div)
	)
	return(rp)
}

ab <- c(300,30,10)

rarity_plot(ab, 1)
rarity_plot(ab, 0)
rarity_plot(ab, -1)
