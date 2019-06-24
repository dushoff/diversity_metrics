
## What is the smallest number of unobserved species consistent
## with a postulated coverage and target value of pair probability?
umin <- function(p, C, pp){
	gap <- pp-C^2*sum(p^2)
	if (gap<=0) stop("C too large in umin (Bt_prob_abu_fiddle)")
	return((1-C)^2/gap)
}

## What is the expected richness of a sample from a community?
## or pseudo-community?
expRich <- function(alpha, n, X=0, u=1){
	α <- alpha[alpha>0]
	return(
		sum(1-(1-α*(1-X))^n)
		+ u*(1-(1-X/u)^n)
	)
}

## What is the expected richness of a hypothetical pseudo-community
## with u=umin (not an integer) and even distribution therein?
## Optionally subtract a postulated richness (for uniroot)
uminRich <- function(C, n, p, pp, r=0){
	u <- umin(p, C, pp)
	return(expRich(n, C*p, 1-C, u)-r)
}

## Our indirect coverage estimate is a coverage that produces a 
## pseudo-community with expected richness equal to observed richness
indCov <- function(x){
	eps <- 1e-3
	r <- length(x)
	n <- sum(x)
	pp <- sum(x*(x-1))/(n*n-1)
	Cmax <- pp/sum((x/n)^2)
	print(r)
	print(uminRich(eps*Cmax, n=n, p=x/n, pp=pp))
	print(uminRich((1-eps)*Cmax, n=n, p=x/n, pp=pp))
	return(uniroot(uminRich, lower=eps*Cmax, upper=(1-eps)*Cmax
		, n=n, p=x/n, pp=pp, r=r
	))
}

## Using sampDiv instead of messing with truemu
## truemu has finite-sample issues; need to discuss with Mike
## Not really tested for ℓ ≠ 1
sampDiv <- function(comm, size, reps, l=1){
	div <- replicate(reps, {
		sam <- sample(1:length(comm), size=size, prob=comm, replace=TRUE)
		ss<-(sapply(1:length(comm), function(y){
			length(which(sam==y))
		}))
		return(dfun(ss, l))
	})
	return(mean(div))
}

