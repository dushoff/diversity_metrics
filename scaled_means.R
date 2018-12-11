library(tidyverse)
library(ggthemes)
library(grid)

theme_set(theme_tufte(base_family = "sans"))

require(scales) # trans_new() is in the scales library
pfun=function(x, pow){
	if (pow==0) return(log(x))
	return(sign(pow)*x^pow)
}

ipfun=function(x, pow, offset=0){
	if (pow==0) return(exp(x))
	return(sign(pow)*(x+offset)^(1/pow))
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
    , transform = function(x) pfun(x, pow)
	, inverse = function(x) ipfun(x, pow)
	, breaks = function(x) prettify(trans_breaks(function(x) pfun(x, pow), function(x) ipfun(x, pow), n =5) (c(0,x)*1.1))
    , domain = c(1, 10000)
)

#add some stupid columns and rows to df to make easier to graph
fancy_rep<-function(df){k<-data.frame(df[rep(1:nrow(df), df$abundance),])
    k %>% group_by(abundance) %>% mutate(gr=1:abundance)}

#Function to plot everything, with some complicated parts inside
rarity_plot <- function(abundance, p){
	rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	)
	div <- summarise(rf, div=ipfun(
		sum(abundance*pfun(rarity, p))/sum(abundance)
		, p
	)) %>% pull(div)
	
	rf2<-fancy_rep(rf) 
	
	rp <- (ggplot(rf2, aes(x=rarity))
	      
	       +geom_point(aes(y=gr-1), size=1.2, shape=22)
	       #line segment instead of stacked boxes
	    # + geom_segment(aes(x=rarity, xend=rarity, y=abundance, yend=0), size=1.6)
	
	    #This deals with clipping, but messes up axis ticks transformation
	   +coord_trans(x=power_trans(pow=p),clip="off")
		#works with ticks but not fulcrum
	    + scale_x_continuous(trans=power_trans(pow=p))
		
	    # fix x-axis at y=0
		+ scale_y_continuous(expand=c(0,0))

		#this is what makes the axis lines... it is just a line segment from min to max of the provided data. the breaks are provided in scale_x_continuous or scale_y_continuous, wich in turn can get them from 
		+ geom_rangeframe(data=data.frame(rarity=c(min(rf$rarity), max(rf$rarity)), abundance=c(0,max(rf$abundance)+10)))
	# + geom_point(aes(x=div, y=-2, color="red"))
	
	    + theme(legend.position="none")
	+labs(y="species abundance")
	
	+ geom_point(x=dfun(ab,1), y=0, color="blue", size=1)
	+ geom_point(x=dfun(ab,0), y=0, color="orange", size=1)
	+ geom_point(x=dfun(ab,-1), y=0, color="red", size=1)
	+ geom_point(x=div, y=-0.05*max(ab), size=6, shape=2)
	)
	return(rp)
}

#provide abundance vectors
ab <- c(20,30,50)
ab<-c(100, 20, 15, 10, 2, 1, 1,1)
# ab<-c(50,20,30,5,3,2)

#actually plot data
# plot_properly<-function(x){
#     gt<-x
# gt$layout$clip[gt$layout$name=="panel"] <- "off"
# grid.draw(gt)
# }
quartz()

rarity_plot(ab,1)
rarity_plot(ab,0)
rarity_plot(ab,-1)

