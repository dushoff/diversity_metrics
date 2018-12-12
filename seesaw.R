library(tidyverse)
library(ggthemes)

## TODO
## Not worry too much
## stacking of repeats
## goff (a formula and maybe an argument)
## Control triangle position
## Worry about limits

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
# Doesn't work (stack) if we have repeated abundance values
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
	goff <- 0.6

	return(ggplot(rfrepeated, aes(x=rarity))
		+ geom_point(aes(y=gr-goff), size=pointsize, shape=22) ## FIX (magic)

	   # fix x-axis
		+ scale_y_continuous(
			expand=c(0,0)
			, limits=c(-50, 1.1*max(rf$abundance))
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

		+ theme(legend.position="none")
		+ labs(y="species abundance")
	)
}

scale_plot <- function(ab, ell){
	div <- dfun(ab, ell)
	print(div)
	return (base_plot(ab) 
		+ geom_point(
			data=tibble(x=div, y=-0.025*max(ab))
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
		data=tibble(x=div, y=0*div)
		, aes(x, y)
		, inherits.aes=FALSE
	))
}

ab <- c(20, 15, 9, 3, 2, 1, 1)
ab <- c(100, 20, 15, 9, 3, 2, 1, 1)

print(scale_plot(ab, 0))

# rarity_plot(ab,1)

