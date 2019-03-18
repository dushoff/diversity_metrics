set.seed(0400)
#sample size
n <- 100
#true binomial probability
p <- 0.72
#iterations
reps <- 5000


#to what extent is this the right question here... aren't problems with the Clopper-Pearson method pretty well known? https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
dtest <- function(x, n, p){
    #get p for both one-sided tests
    p1 <- binom.test(x, n, p = p
                     , alternative = c("less")
    )$p.value
    p2 <- binom.test(x, n, p = p
                     , alternative = c("greater")
    )$p.value
    print(c(p1=p1, p2=p2))
    #deal with ties for checkplots
    return((p1 + 1 - p2)/2)
}

checkplot <- function(plist, main="checkplot"
                      , xlab="P", boxes=40, col="gray", lwd=1
){
    hist(plist
         , breaks=seq(0, 1, length.out=1+boxes)
         , main = main, xlab=xlab
         , probability=TRUE
         , col=col
    )
    abline(h=1, lty=2, lwd=lwd)
}


bvec <- replicate(reps, {
    #generate a bunch of random numbers of success for size n, prob p
    x <- rbinom(1, size=n, prob=p)
    #use a pearson chi-squared test to see chance that x is less than expected given n, p
    prop.test(x, n, p = p
              , alternative = c("less")
    )$p.value
})
 #using same n and p, use the two sided test above with
dvec <- replicate(reps, {
    x <- rbinom(1, size=n, prob=p)
    dtest(x, n, p)
})

print(checkplot(bvec, "binom.test", boxes=10))
print(checkplot(dvec, "Tie corrected", boxes=10))

#to keep on this might be good to look at https://stats.stackexchange.com/q/5206/171222 and also the r package binom