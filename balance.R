library(tidyverse)
library(ggthemes)

## TODO
## OPTIONAL: color the species (so that stacking becomes clear)

#figure out how to shrink according to viewport rather than device size, and do it to all things rather than just some, would allow easier integration into multi-panel figures

## might want to do get text and axis widths to scale with device/viewport. currently base_size argument can control manually

## flexibly design multipaneled figures: https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

## think about squeezing when rarities aren't equal but are close s.t. boxes overlap... maybe go back to lines (or make an option?) (Or a smart option that can calculate whether overlap occurs?)

## along similar lines, consider including an optional argument for plotting in local graphics device vs. the nice version to pdf, where some compromises in the former to enable flexibility, but don't make those sacrifices when printing to file. 

## select colors for reference points http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3


library(scales) # trans_new() is in the scales library

epsPretty <- 0
offStart <- 0

#transformation and back-transformation functions
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

#function for scales
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

base_plot <- function(abundance, pointScale
                      , fill_col="lightgrey" #can set to match communities
                      , y_extent=max(max(abundance),15) #how tall to draw y
                      , x_max=sum(abundance)/min(abundance) #plots a point to extend x
                      , x_min=sum(abundance)/max(abundance) # point to exted x
                      , base_size=24 #controls text size, default for 7" sq plotting device
                      , noco=1 #number of columns, shrinks text and point size proportional to number of panels
                      ){
    #0.0353 is approximate points to cm conversion (a little less than 3 pts per mm)
    #11.25 is empirically derived scaling factor. Seems like stuff below axis is about 2.5* height of 1 line of text
    pointScale<-(11.25*(min(dev.size("cm"))/noco-(2.5*0.0353*base_size)))
    pointsize <- pointScale/(y_extent)
	
    #make plotting data
    rf <- tibble(names = as.factor(1:length(abundance))
		, abundance
		, rarity = sum(abundance)/abundance
	)
	rfrepeated <-fancy_rep(rf) 

	#0.5; shape is centered on  x,y; offset so it rests upon it
	goff <- 0.5

	#ggplot command to generate basic plot object
	base <- (ggplot(rfrepeated, aes(x=rarity, y=abundance))
	    #bricks
		+ geom_point(aes(y=gr-goff, alpha=0.2), size=pointsize, fill=fill_col
		             , shape=22, color="black", stroke=0.5) 
		# plank
		+ geom_segment(
			aes(x, y, xend=xend, yend=yend)
			, data=data.frame(
				x=c(min(rf$rarity))
				, y=c(0)
				, xend=c(max(rf$rarity))
				, yend=c(0)
			)
		)
		
#fix plank location and add space above and below data range
		+scale_y_continuous(
		    expand=c(0,0)
		    , limits=c(y_extent-1.1*y_extent, 1.1*y_extent)
		)
    	+ labs(y="individuals")
	)
	return(theme_plot(base, base_size=base_size, noco=noco))
}

theme_plot <- function(p, base_size=24, noco=1,...){
	return(p
		+ theme_tufte(base_family = "sans", base_size=base_size/noco)
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

#rescales x-axis
scale_plot <- function(ab, ell, fill_col="lightgrey", y_extent=max(max(ab), 15)
                       , x_max=sum(ab)/min(ab), x_min=sum(ab)/max(ab), noco=1,...){
	return (base_plot(ab, fill_col=fill_col, y_extent=y_extent
	                  , x_max=x_max, x_min=x_min, noco=noco, ...) 
		+ scale_x_continuous(trans=power_trans(pow=ell))
		#allows for x min and max points to determine axes
		+ geom_point(aes(x,y), data=tibble(x=c(x_max, x_min), y=c(0,0)), color="white", alpha=0)
		)
}

#plots reference points at means with power ell
mean_points <- function(ab, ell, noco=1){
    ab<-ab[ab!=0]
	div <- Vectorize(dfun, vectorize.args=("l"))(ab, ell)
	return(geom_point(
		data=tibble(x=div, y=0*div, clr=1:length(div))
		, aes(x, y, color=as.factor(clr))
		,size=0.2*min(dev.size("cm"))/noco
	))
}

#plot the fulcrum
fulcrum<-function(ab, ell, y_extent=max(max(ab), 15), x_max=1
                  , x_min=1, fill_col="light_grey", base_size=24, noco=1){
    ab<-ab[ab!=0]
    div <- dfun(ab, ell)
    print(div)
    return(geom_point(
        data=tibble(x=div, y=-0.035*y_extent) # gets fulcrum point close. 
        , size=(0.48*min(dev.size("cm"))-(2.5*0.0353*base_size))/noco #scales with plotting device and number of columns
        # , size=rel(0.3)
        , shape=17
        , aes(x, y) 
    )
    )
}

#construct the full plot for scale ell, with reference means=means
rarity_plot <- function(ab, ell, means=-1:1, noco=1, ...){
    ab<-ab[ab!=0]
	return(
		scale_plot(ab, ell, noco=noco,...) 
		+ mean_points(ab, means, noco=noco)
		+ fulcrum(ab, ell, noco=noco, ...)
		+ scale_color_brewer(type="qual", palette="Dark2") 
	)
}

#one option for plotting all three plots for l=-1:1, with reference points at integer l 
rarity_series <- function(ab, lrange=-1:1, means=lrange,...){
	for(l in lrange){
		print(rarity_plot(ab, l, means,...))
	}
}

#convenience function to omit all y-axis elements for constructing multi-panel plots
omit_y<-function(p){
    return(p
           +theme(axis.text.y=element_text(color="white")
                  , axis.title.y=element_text(color="white")
                  , axis.ticks.y = element_blank(), axis.line.x=element_line(
                      colour = 'black', size=0.2, linetype='solid'
                  )))
}


#some SADs to play with

# ab <- c(20, 15, 9, 3, 2, 1, 1) #includes stacking
ab<-c(20,8,5,4,2,1) #candidate for user's guide
# ab <- c(100, 20, 15, 9, 3, 2, 1, 1)
# ab<-c(50,30,20,0,0,0)
# ab<-c(4,3,2)   
# ab <- c(20, 15, 9, 3, 2, 1, 1,0,0)
# ab <- c(200,100, 20, 15, 9, 3, 2, 1, 1)
# ab<-floor(exp(rnorm(50, 4,1.5)))

quartz()
p<-rarity_plot(ab, 1, fill_col="blue", x_min=1, x_max=45, noco=3, base_size=12)
grid.arrange(p, omit_y(p), omit_y(p), p, omit_y(p), omit_y(p), p, omit_y(p), omit_y(p))
grid.arrange(p,p,p,p,p,p,p,p)
# rarity_series(ab=ab, 1:-1)
