### this is stuff from JD checkplot work that was deleted from notebook
## Data to check with checking scripts

library(tidyverse)

points <- 8
reps <- 20
reps <- 2000

normList <- rerun(reps, rnorm(points)) ########## rerun is purrr's version of replicate, an alternative to map.
expList <- rerun(reps, rexp(points)-1)

repList <- lapply(normList, function(v){return(c(v, v, v))}) #just makes very record 3 records

multT <- function(datList){
    df <- as.data.frame(t(sapply(datList, function(d){
        t <- t.test(d) #run a t-test on each bunch of deviates
        p <- (1-sign(t$statistic)*(1-t$p.value))/2 #turning one sided test into a two sided test.
        return(c(p, t$est, t$conf.int))
    })))
    names(df) <- c("p", "est", "lower", "upper")
    return(df)
}

normT <- multT(normList)
expT <- multT(expList)
normX <- multT(repList)

# Wmin = 0.95
# 
# for (tag in names(stats)){
#     W = sprintf("%4.2f", mean(stats[[tag]]$W))
#     print(checkplot(stats[[tag]], tag=paste(tag, "W ~", W)))
#     print(checkplot(stats[[tag]], Wmin=Wmin, tag=paste(tag, "W > ", Wmin)))
# } 
# 
# checkplot <- function(stats, breaks=seq(0,1,0.05), tag="", Wmin=0){
#     stats <- filter(stats, W>Wmin)
#     return(ggplot(stats, aes(p))
#            + geom_histogram(breaks=breaks)
#            + geom_hline(yintercept=nrow(stats)/(length(breaks)-1))
#            + ggtitle(tag)
#     )
# }

## rangePlots are named for their order functions:
## slug, blob and milli (for millipede)
## slug is currently preferred
rangePlot <- function(tf, orderFun=slug, alpha=0.05 #note that alpha is both an opacity and the complement of the confidence level here.
                      , opacity=0.2, fatten=0.1, title="Range plot"
){
    return(ggplot(
        orderFun(tf)
        , aes(x=quantile, y=est, ymin = lower, ymax=upper)
    )
    + geom_pointrange(alpha=opacity, fatten=fatten)
    + geom_hline(yintercept=0)
    + geom_vline(
        xintercept=c(alpha/2, 1-alpha/2)
        , lty=2, col="red"
    )
    + xlab("index")
    + ylab("estimate")
    + ggtitle(title)
    )
}

milli <- function(tf){ #mili quantiles are the simplest kind of order statistic
    numEst <- nrow(tf)
    return(tf
           %>% arrange(est)
           %>% mutate(
               quantile=((1:numEst)-1/2)/numEst
           )
    )
}

blob <- function(tf){ #blob quantiles are given by the ends of the confidence intervals instead of by the estimate
    numEst <- nrow(tf)
    return(tf
           %>% mutate(
               side = sign(est-median(est))
               , pos = ifelse(side>0, lower, upper)
           )
           %>% arrange(side, pos)
           %>% mutate(
               quantile=((1:numEst)-1/2)/numEst
           )
    )
}

slug <- function(tf){
    numEst <- nrow(tf)
    return(tf
           %>% arrange(est)
           %>% mutate(
               estQ=((1:numEst)-1/2)/numEst #some kind of order statistic
               , pos = estQ*(lower-median(lower)) #some way of centering the order statistic based on both the upper and lower quantiles
               +(1-estQ)*(upper-median(upper))
           )
           %>% arrange(pos)
           %>% mutate(
               quantile=((1:numEst)-1/2)/numEst
           )
    )
}

rangePlot(normT, orderFun=blob)
