library(tidyverse)
library(ggthemes)

## TODO
## Not worry too much
## stacking of repeats
## goff (a formula and maybe an argument)
## How did you make normal axis lines disappear? Can we get them back?

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
		%>% group_by(abundance) %>% mutate(gr=1:abundance[[1]])
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

	base <- (ggplot(rfrepeated, aes(x=rarity, y=abundance))
		+ geom_point(aes(y=gr-goff), size=pointsize, shape=22)

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
		data=tibble(x=div, y=0*div, c=1:length(div))
		, aes(x, y, color=c)
		, inherits.aes=FALSE
	))
}

rarity_plot <- function(ab, ell, lrange=-1:1){
	return(
		scale_plot(ab, ell) + mean_points(ab, lrange)
	)
}

rarity_series <- function(ab, lrange=-1:1){
	for(l in lrange){
		print(rarity_plot(ab, l, lrange))
	}
}

ab <- c(100, 20, 15, 9, 3, 2, 1, 1)
ab <- c(20, 15, 9, 3, 2, 1, 1)

rarity_series(ab, 1:-1)
