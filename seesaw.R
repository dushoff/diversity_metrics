library(tidyverse)
library(ggthemes)

theme_set(theme_tufte(base_family = "sans"))
library(scales) # trans_new() is in the scales library

epsPretty <- 0
offStart <- 0

pfun=function(x, pow, offset=offStart){
	if (pow==0) return(log(x))
	r <- sign(pow)*(x+offset)^pow
	## cat("pfun", x, "pow", pow, "v", r, "\n")
	return(r)
}

ipfun=function(x, pow, offset=offStart){
	if (pow==0) return(exp(x))
	r <- sign(pow)*(x)^(1/pow)
	## cat("ipfun", x, "pow", pow, "v", r, "\n")
	return(r)
}

# Diversity function 
dfun<-function(ab, l){
	rp <- ab/sum(ab)
	return(ipfun(sum(rp*pfun(1/rp, l)),l))
}

# round numbers, more aggressively the larger they are
prettify <- function(breaks){
    digits <- -floor(log10(abs(breaks))) + 1
    digits[breaks == 0] <- 0
    return(round(breaks, digits = digits))
}

power_trans = function(pow) trans_new(name="power"
   , transform = function(x) pfun(x, pow)
	, inverse = function(x) ipfun(x, pow)
	, breaks = function(x) prettify(
		trans_breaks(
			function(x) pfun(x, pow), function(x) ipfun(x, pow), n = 5
		)(c(epsPretty,x)*1.1)
	)
   , domain = c(1, 10000)
)

# replicate to make stacks with geom_point
fancy_rep<-function(df){
	return(data.frame(df[rep(1:nrow(df), df$abundance),])
		%>% group_by(abundance) %>% mutate(gr=1:abundance[[1]])
	)
}

#Function to plot everything, with some complicated parts inside
rarity_plot <- function(abundance, p){
	rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	)
	rflist <-fancy_rep(rf) 

	div <- (
		summarise(rf, div=ipfun(
			sum(abundance*pfun(rarity, p))/sum(abundance)
			, p
		))
		%>% pull(div)
	)
	
	
	rp <- (ggplot(rflist, aes(x=rarity))
	      
	       +geom_point(aes(y=gr-0.6), size=2, shape=22)
	       #line segment instead of stacked boxes
	    # + geom_segment(aes(x=rarity, xend=rarity, y=abundance, yend=0), size=1.6)

	    #This deals with clipping, but messes up axis ticks transformation
	   ## + coord_trans(x=power_trans(pow=p),clip="off")
	   + coord_cartesian(clip="off")
		#works with ticks but not fulcrum
	    + scale_x_continuous(trans=power_trans(pow=p))

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
		
    	# Fulcrum
		+ geom_point(x=div, y=-0.025*max(ab), size=6, shape=2)
	)
	return(rp)
}

ab<-c(100, 20, 15, 9, 3, 2, 1)

rarity_plot(ab,1)
