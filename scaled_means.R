library(tidyverse)
library(ggthemes)
library(grid)

theme_set(theme_tufte(base_family = "sans"))

require(scales) # trans_new() is in the scales library
#need the reverse bit for the scaling of the axes to get them in the right orientation
pfun=function(x, pow, rever=F){
	if (pow==0) return(log(x))
    if (rever==T){
    if (pow<(0)) return(-x^pow)
        return(x^pow)}
	return(x^pow)
}

ipfun=function(x, pow, rever=F){
	if (pow==0) return(exp(x))
    if(rever==T){if (pow<(0)) return(-x^(1/pow))
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
	# rf2<-data.frame("gr"=as.factor(1:nrow(fancy_rep(rf))),fancy_rep(rf))
	rf2<-fancy_rep(rf) 
	
	rp <- (ggplot(rf2, aes(x=rarity))
	       #This makes almost ok stacks of boxes. not ok because of scaling transformations
	       +geom_point(aes(y=gr-1), size=0.25)
	       # +geom_bar(aes(group=gr, width=0.1),fill="grey", colour="black", size=0.1)
	       # + geom_vline(xintercept=div, color="red", size=1.1)
	  
	       
	       # + geom_vline(xintercept=dfun(ab,-1), color="purple", linetype=4, size=1)
	       # + geom_vline(xintercept=dfun(ab,0), color="green", linetype=5, size=1)
	     
	  #This makes a nice line that doesn't have issues with scaling
	# + geom_segment(aes(x=rarity, xend=rarity, y=abundance, yend=0), size=1.6)
	
	   # +coord_cartesian(clip="off")
	
	#this one seems to work better with the labels
	+ scale_x_continuous(trans=power_trans(pow=p))
		
	#the expand=c(0,0) is what fixes x-axis in place at y=0
		+ scale_y_continuous(expand=c(0,0))

		#this is what makes the axis lines... it is just a line segment from min to max of the provided data. teh breaks are provided in scale_x_continuous or scale_y_continuous, wich in turn can get them from 
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
plot_properly<-function(x){
    gt<-x
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)
}

plot_properly(rarity_plot(ab,1))
plot_properly(rarity_plot(ab,0))
plot_properly(rarity_plot(ab,-1))
