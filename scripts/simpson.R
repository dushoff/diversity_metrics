#mini is going to be the way to add a pile of rare species while maintaining a fixed Simpson diversity
mini <- 100
seed <- 100

set.seed(seed)

#takes a vector of abundances and returns Simpson's concentration
sExact <- function (com){
	c <- com/sum(com)
	return(1/(sum(c^2)))
}

# takes vector of sample abundances and returns Simpson's estimator of Simpsons concentration
sApp <- function(samp){
	n <- sum(samp)
	return(1/(
		sum((samp/n)*((samp-1)/(n-1)))
	))
}

# takes a vector of species abundances (com) and a number of individuals to "subsample", actually returns a bootstrap-type subsample (i.e. with replacement). Not clear why it should be with replacement at this point.
cSamp <- function(com, size){
	sp <- sample(1:length(com), size=size, prob=com, replace=TRUE)
	tab <- table(sp)
	return(as.vector(tab))
}

# repeatedly bootstraps com to size=size, returns Simpson's estimator
appRange <- function(com, size, reps){
	app <- numeric(reps)
	for (i in 1:reps){
		app[[i]] <- sApp(cSamp(com, size))
	}
	return(app)
}

#provides 0.025 and 0.095 quantiles for bootstrapped estimates... Is the North et al. 2002 thing relevant here?
ci <- function(l, P=0.025){
	reps <- length(l)
	offset <- floor(P*reps)
	return(c(
		low=sort(l)[offset],
		high=sort(l)[1+reps-offset]
	))
}

#sampling with replacement, so comms can be 4 individuals
com1 <- c(1, 1, 1, 1)
com2 <- c(1, rep(1/(mini-1), mini))

#40 individuals, 1000 times
r1 <- appRange(com1, 40, 1000)
r2 <- appRange(com2, 40, 1000)


print(mean(r1))
print(mean(r2))

print(ci(r1))
print(ci(r2))

print(mean(1/r1))
print(mean(1/r2))

print(hist(1-1/r1, main="Even", xlab="index"))
print(hist(1-1/r2, main="Uneven", xlab="index"))

plot(table(1-1/r1), main="Even", xlab="index")
print(table(1-1/r1), main="Even", xlab="index")

#this is a cool function to turn double/float type numbers into a ratio of two integers
print(MASS::fractions(sort(unique(1-1/r1))))

print(hist(r1, main="Even", xlab="diversity"))
print(hist(r2, main="Uneven", xlab="diversity"))
