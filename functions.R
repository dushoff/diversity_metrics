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
