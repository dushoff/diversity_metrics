powPop <- function(rich, a){
	p <- (1:rich)^(-a)
	return(p/sum(p))
}

invFun <- function(rar, l){
	if (l==0) return(exp(rar))
	return(rar^(1/l))
}

powFun <- function(rar, l){
	if (l==0) return(log(rar))
	return(rar^l)
}

popDiversity <- function(p, l){
	p <- p/sum(p)
	tr <- powFun(1/p, l)
	tr[p==0] <- 0
	return(invFun(sum(p*tr), l))
}

ssrDiversity <- function(p){
	return(c(
		Simpson = popDiversity(p, -1)
		, Shannon  = popDiversity(p, 0)
		, Richness  = popDiversity(p, 1)
	))
}

rprime <- function(a, b){
	if(b==1) return(TRUE)
	if(b==0) return(FALSE)
	return (rprime(b, a%%b))
}

mix <- function(n){
	rp <- sapply(1:floor(n/2), function(x){rprime(n, x)})
	step <- max(which(rp))
	return(1 + ((step*(1:n)) %% n))
}

piePop <- function(p){
	n <- length(p)
	# return(mix(n))
	d <- data.frame(
		label = as.factor(1:n)
		, p=sample(p)
	)

	colorFun <- colorRampPalette(brewer.pal(9, "Set1")) 
	colorRamp <- colorFun(n)[mix(n)]

	return(
		ggplot(d, aes(x=factor(1), fill=label, y=p))
		+ geom_bar(stat="identity")
		+ coord_polar(theta="y")
		+ theme(legend.position="none")
		+ theme(
			line = element_blank(),
			text = element_blank(),
			title = element_blank()
		)
		+ xlab("")
		+ ylab("")
		+ scale_fill_manual(values=colorRamp)
	)
}

pieStats <- function(p){
	print(piePop(p))
	return(ssrDiversity(p))
}
