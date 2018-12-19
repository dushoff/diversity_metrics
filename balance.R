library(tidyverse)
library(ggthemes)

## TODO
## OPTIONAL: color the species (so that stacking becomes clear)
## fix box size for small max abundance so it doesn't e.g. cross y-axis
## think about squeezing when rarities aren't equal but are close s.t. boxes overlap
## select colors for reference points http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
## switch plotting order so that fulcrum point sits above reference points

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

power_trans = function(pow) trans_new(name="power"
   , transform = function(x) pfun(x, pow)
	, inverse = function(x) ipfun(x, pow)
	, breaks = function(x) prettify(
		trans_breaks(
			function(x) pfun(x, pow), function(x) ipfun(x, pow), n = 6
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

base_plot <- function(abundance, pointScale=200){
    
	rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	)
	rfrepeated <-fancy_rep(rf) 

	pointsize <- pointScale/max(abundance)
	
	#This pretty much has to be 0.5 because the shape is centered on its x- and y-locations, but we want to offset so it rests upon it
	goff <- 0.5

	base <- (ggplot(rfrepeated, aes(x=rarity, y=abundance))
		+ geom_point(aes(y=gr-goff, alpha=0.2), size=pointsize, fill="lightgrey", shape=22, color="black", stroke=0.5) #some stylistic things to deal with squeezing
		
		# make plank
		+ geom_segment(
			aes(x, y, xend=xend, yend=yend)
			, data=data.frame(
				x=c(min(rf$rarity))
				, y=c(0)
				, xend=c(max(rf$rarity))
				, yend=c(0)
			)
		)
# adding this in let the boxes fill the y-space pretty well without gaps, don't think it ruined appearance of y-axis
		+scale_y_continuous(
		    expand=c(0,0)
		    , limits=c(max(rf$abundance)-1.1*max(rf$abundance), 1.1*max(rf$abundance))
		)
		+ labs(y="species abundance")
	)
	return(theme_plot(base))
}

theme_plot <- function(p){
	return(p
		+ theme_tufte(base_family = "sans")
		+ theme(legend.position="none")
		+ theme(
			axis.line.x = element_line(
				colour = 'black', size=0.2, linetype='solid'
			), axis.line.y =  element_line(
				colour = 'black', size=0.2, linetype='solid'
			)
		)
	)
}

scale_plot <- function(ab, ell){
	div <- dfun(ab, ell)
	print(div)
	return (base_plot(ab) 
		+ geom_point(
			data=tibble(x=div, y=-0.028*max(ab))# don't recall why 0.028, but it gets fulcrum point just right. 
			, size=6, shape=2
			, aes(x, y)
		)
		+ scale_x_continuous(trans=power_trans(pow=ell))
		+ coord_cartesian(clip="off")
	)
}

mean_points <- function(ab, ell){
	div <- Vectorize(dfun, vectorize.args=("l"))(ab, ell)
	return(geom_point(
		data=tibble(x=div, y=0*div, clr=1:length(div))
		, aes(x, y, color=as.factor(clr))
	))
}

rarity_plot <- function(ab, ell, lrange=-1:1){
	return(
		scale_plot(ab, ell) 
		+ mean_points(ab, lrange)
		+ scale_color_brewer(type="qual", palette="Dark2") #playing with color choices
	)
}

rarity_series <- function(ab, lrange=-1:1){
	for(l in lrange){
		print(rarity_plot(ab, l, lrange))
	}
}

#some SADs to play with

# ab <- c(20, 15, 9, 3, 2, 1, 1)
ab <- c(100, 20, 15, 9, 3, 2, 1, 1)
# ab<-c(50,30,20)
# ab<-c(4,3,2)
# ab <- c(20, 15, 9, 3, 2, 1, 1)
# ab <- c(200,100, 20, 15, 9, 3, 2, 1, 1)
# ab<-floor(exp(rnorm(50, 4,1.5)))

# quartz()
rarity_plot(ab, 1)

# quartz()
rarity_plot(ab, 0)

# quartz()
rarity_plot(ab, -1)


