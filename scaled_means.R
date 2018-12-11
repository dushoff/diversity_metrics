
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
<<<<<<< HEAD
	       #This makes almost ok stacks of boxes. not ok because of scaling transformations
	       +geom_point(aes(y=gr-1), size=0.25)
	       # +geom_bar(aes(group=gr, width=0.1),fill="grey", colour="black", size=0.1)
	       # + geom_vline(xintercept=div, color="red", size=1.1)
	  
	       
	       # + geom_vline(xintercept=dfun(ab,-1), color="purple", linetype=4, size=1)
	       # + geom_vline(xintercept=dfun(ab,0), color="green", linetype=5, size=1)
	     
	  #This makes a nice line that doesn't have issues with scaling
	# + geom_segment(aes(x=rarity, xend=rarity, y=abundance, yend=0), size=1.6)
	
	   # +coord_cartesian(clip="off")
=======
	      
	       +geom_point(aes(y=gr-0.6), size=2, shape=22)
	       #line segment instead of stacked boxes
	    # + geom_segment(aes(x=rarity, xend=rarity, y=abundance, yend=0), size=1.6)
>>>>>>> 1975ceb6799d7929c31db433761372e0c499956f
	
	    #This deals with clipping, but messes up axis ticks transformation
	   +coord_trans(x=power_trans(pow=p),clip="off")
		#works with ticks but not fulcrum
	    # + scale_x_continuous(trans=power_trans(pow=p))
		
	    # fix x-axis at y=0
		+ scale_y_continuous(expand=c(0,0))
		
		#this is what makes the axis lines... it is just a line segment from min to max of the provided data. breaks work with scale_x_continuous/ scale_y_continuous, which can use the whole trans argument. coord_trans doesn't play nice with that
		+ geom_rangeframe(aes(rarity, abundance)
		                  , data=data.frame(rarity=c(min(rf$rarity), max(rf$rarity)), abundance=c(0,max(rf$abundance)*1.1))
		                  , inherit.aes = F)
	
		+ theme(legend.position="none")
		+ labs(y="species abundance")
		
		#add colored marks for each mean
		+ geom_point(x=dfun(ab,1), y=0, color="#C77CFF", size=2)
		+ geom_point(x=dfun(ab,0), y=0, color="#00BFC4", size=2)
		+ geom_point(x=dfun(ab,-1), y=0, color="#F8766D", size=2)
		
    	#This is the fulcrum
		+ geom_point(x=div, y=-0.025*max(ab), size=6, shape=2)
	)
	return(rp)
}

#provide abundance vectors
# ab <- c(20,30,50)
ab<-c(100, 20, 15, 10, 2, 1, 1,1)
# ab<-c(50,20,30,5,3,2)


# quartz()
pdf(file="rarity_seesaws_1.pdf")
rarity_plot(ab,1)
rarity_plot(ab,0)
rarity_plot(ab,-1)
dev.off()
