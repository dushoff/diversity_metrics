library(tidyverse)
library(ggthemes)
library(scales) # trans_new() is in the scales library

## TODO
## OPTIONAL: color the species (so that stacking becomes clear)

## might want to do get text and axis widths to scale with device/viewport. currently base_size argument can control manually

## think about squeezing when rarities aren't equal but are close s.t. boxes overlap... smart option that can calculate whether overlap occurs?

## select colors for reference points http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3

epsPretty <- 0 # probably can delete these?
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

#function for scale transformation
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
           %>% group_by(abundance) %>% mutate(gr=1:length(abundance), inds=rep(length(abundance), length(abundance)))
    )
}

base_plot <- function(abundance, pointScale
                      , fill_col="lightgrey" #can set to match communities
                      , y_extent=max(max(abundance),15) #how tall to draw y
                      , x_max=sum(abundance)/min(abundance) #plots a point to extend x
                      , x_min=sum(abundance)/max(abundance) # point to exted x
                      , base_size=24 #controls text size, default for 7" sq plotting device
                      , noco=1 #number of columns, shrinks text and point size proportional to number of panels
                      , lines=F
                      , verbose=T
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
	         +(if(lines==T){
	             #line segments
	             geom_segment(aes(x=rarity, xend=rarity, y=inds, yend=0)
	                           , color=fill_col
	                           , size=1
	                           )
	         } else{
	    
	   #bricks
	    geom_point(aes(y=gr-goff, alpha=0.2), size=pointsize, fill=fill_col
		             , shape=22, color="black", stroke=0.5) 
	         })
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
		    , limits=c(y_extent-1.05*y_extent, 1.05*y_extent)
		)
    	+ labs(y="individuals")
	)
	return(theme_plot(base, base_size=base_size, noco=noco))
}

#set preferences for axes etc.
theme_plot <- function(p, base_size=24, noco=1,...){
	return(p
		+ theme_tufte(base_family = "sans", base_size=base_size/noco)
		+ theme(legend.position="none")
		+ theme( 
		    axis.text=element_text(color="black") # better than Wickham's grey?
		    , axis.line.x = element_line(colour = 'black'
		                                 , size=0.2, linetype='solid')
			, axis.line.y = element_line(colour = 'black'
			                             , size=0.2, linetype='solid')
		)
	)
}

#starts with baseplot, rescales x-axis, adds space to allow matching scales between comms
scale_plot <- function(ab, ell, fill_col="lightgrey", y_extent=max(max(ab), 15)
                       , x_max=sum(ab)/min(ab), x_min=sum(ab)/max(ab), noco=1, lines=F, ...){
	return (base_plot(ab, fill_col=fill_col, y_extent=y_extent
	                  , x_max=x_max, x_min=x_min, noco=noco, lines=lines, ...) 
		+ scale_x_continuous(trans=power_trans(pow=ell), labels=signif)
		+ geom_point(aes(x,y) #allows for x min and max points to determine axes
		             , data=tibble(x=c(x_max, x_min), y=c(0,0))
		             , color="white", alpha=0)
		)
}

#plots reference points at means with power "ell"
mean_points <- function(ab, ell, noco=1){
    ab<-ab[ab!=0]
	div <- Vectorize(dfun, vectorize.args=("l"))(ab, ell)
	return(geom_point(
		data=tibble(x=div, y=0*div, clr=1:length(div))
		, aes(x, y, color=as.factor(clr))
		, size=0.2*min(dev.size("cm"))/noco
	))
}

#plot the fulcrum
fulcrum<-function(ab, ell, y_extent=max(max(ab), 15), x_max=1
                  , x_min=1, fill_col="light_grey"
                  , base_size=24, noco=1, verbose=T){
    
    ab<-ab[ab!=0]
    div <- dfun(ab, ell)
    
    if(verbose==T){
        print(c("diversity= ", div, " community size= ", sum(ab)
                , " max observed rarity= ", sum(ab)/min(ab)
                , " min observed rarity= ", sum(ab)/max(ab)))}
    
    return(geom_point(
        data=tibble(x=div, y=-0.03*y_extent) # gets fulcrum point close. 
        , size=(0.48*min(dev.size("cm"))/noco-(2.5*0.0353*base_size)) #scales with plotting device and number of columns
        # , size=rel(0.3)
        , shape=17
        , aes(x, y) 
    )
    )
}

#construct the full plot for scale ell, with reference means=means
rarity_plot <- function(ab, ell, means=-1:1, noco=1, lines=F, ...){
    ab<-ab[ab!=0]
    print(cat("     rarity plot expects a square viewport and resizes points based on\n     min(dev.size() and noco (for number of columns).\n     selecting lines=T will plot stacks of individuals as a line element,\n     which tends to be more robust to window size.\n     lines=T may be the best way to deal with overplotting,\n     which results from several species with similar but not identical rarities.\n "))
	return(
		scale_plot(ab, ell, noco=noco, lines=lines,...) 
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

#convenience function to omit y-axis elements for constructing multi-panel plots... mostly 
omit_y<-function(p){
    return(p
           +theme(axis.text.y=element_text(color="white")
                  , axis.title.y=element_text(color="white")
                  , axis.ticks.y = element_line(color="white")
                  , axis.line.y = element_line(color="white")
                  , axis.line.x=element_line(
                      colour = 'black', size=0.2, linetype='solid'
                    )
                  )
           )
}


#some SADs to play with

# ab <- c(20, 15, 9, 3, 2, 1, 1) #includes stacking
# ab<-c(20,8,5,4,2,1) #candidate for user's guide
# ab <- c(100, 20, 15, 9, 3, 2, 1, 1)
# ab <- c(50,30,20,0,0,0)
# ab <- c(4,3,2)
# ab <- c(20, 15, 9, 3, 2, 1, 1,0,0)
ab <- c(200,100, 20, 15, 9, 3, 2, 1, 1)
# ab <- floor(exp(rnorm(50, 4,1.5)))

quartz(height=2, width=2)
rarity_plot(ab,0, lines=T, fill_col="red", base_size=9, verbose=T)

# p<-rarity_plot(ab, 1, fill_col="blue", x_min=1, x_max=45, noco=3, base_size=12)
# 
# grid.arrange(p, omit_y(p), omit_y(p), p, omit_y(p), omit_y(p), p, omit_y(p), omit_y(p))
# 
# grid.arrange(p,p,p,p,p,p,p,p)

# rarity_series(ab=ab, 1:-1)
