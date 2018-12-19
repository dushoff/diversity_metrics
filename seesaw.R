#script to take vector of abundances and chosen "l" and generate seesaw plots. Set to work in a pdf graphics device (e.g. pdf, quartz, but won't always look nice in RStudio's graphics device.)

library(tidyverse)
library(ggthemes)

## TODO

## box size for small max abundances


theme_set(theme_tufte(base_family = "sans"))
library(scales) # trans_new() is in the scales library

epsPretty <- 0
offStart <- 0

pfun=function(x, pow, offset=offStart){
	if (pow==0) return(log(x))
	r <- sign(pow)*(x+offset)^pow
	return(r)
}

ipfun=function(x, pow, offset=offStart){
	if (pow==0) return(exp(x))
	r <- sign(pow)*(x)^(1/pow)
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
    return((round(breaks, digits = digits))[round(breaks, digits = digits)!=(-Inf)]) #klugey fix to -Inf
}

#define how to transform axes back and forth
power_trans = function(pow) trans_new(name="power"
   , transform = function(x) pfun(x, pow)
	, inverse = function(x) ipfun(x, pow)
	, breaks = function(x) prettify(
		trans_breaks(
			function(x) pfun(x, pow), function(x) ipfun(x, pow), n = 6 #I think this looks pretty good?
		)(c(epsPretty,x)*1.1)
	)
   , domain = c(1, 10000)
)

# replicate to make stacks with geom_point
fancy_rep<-function(df){
	return(data.frame(df[rep(1:nrow(df), df$abundance),])
		%>% group_by(abundance) %>% mutate(gr=1:length(abundance))
	)
}

base_plot <- function(abundance, pointScale=200){ #200 seems to look pretty good

	rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	    )
	
	rfrepeated <-fancy_rep(rf) 

	pointsize <- pointScale/max(abundance)
	goff <- 0.5 #not magic, the center of a box should be at a positive integer, and we've resized the boxes so they meet at the midpoint. So subtracting 0.5 moves the stack from 0.5 to 0, every time.
	
	return(ggplot(rfrepeated, aes(x=rarity))
		
	   # fix x-axis
		+ scale_y_continuous(
			expand=c(0,0)
			, limits=c(max(rf$abundance)-1.1*max(rf$abundance), 1.1*max(rf$abundance))
		)
        
		# make axis lines with line segments 
		+ geom_segment(
			aes(x, y, xend=xend, yend=yend)
			, data=data.frame(
				x=c(min(rf$rarity))
				, y=c(0)
				, xend=c(max(rf$rarity))
				, yend=c(0)
			)
		)
		
		+ geom_point(aes(y=gr-goff), size=pointsize, shape=22, fill="lightgrey", alpha=0.2) 
		#clean up plot
		+ theme(legend.position="none")
		+ labs(y="species abundance \n")
	)
}

scale_plot <- function(ab, ell){
	div <- dfun(ab, ell)
	# print(div)
	return (base_plot(ab) 
	        #add fulcrum
		+ geom_point(
			data=tibble(x=div, y=-0.028*max(ab))
			, size=6, shape=2
			, aes(x, y)
		)
		+ scale_x_continuous(trans=power_trans(pow=ell))
		+ coord_cartesian(clip="off")
	)
}

mean_points <- function(ab, l_scale,ell=-1:1){
	div <- Vectorize(dfun, vectorize.args=("l"))(ab, ell)
	return(
	    (scale_plot(ab,l_scale) 
	       +geom_point(
		        data=tibble(x=div, y=0*div)
		        , aes(x, y, color=as.factor(ell))
		        )    
            )
	+scale_color_colorblind()
	
	)
}

#abundance distribution
# ab<-c(50,30,20)
# ab<-c(4,3,2)
# ab <- c(20, 15, 9, 3, 2, 1, 1)
ab <- c(200,100, 20, 15, 9, 3, 2, 1, 1)
# ab<-floor(exp(rnorm(50, 4,1.5)))

#plot the data
# quartz()
print(mean_points(ab,-1))
print(mean_points(ab,0))
print(mean_points(ab,1))
