checkplot <- function(stats, breaks=seq(0,1,0.05), tag="", Wmin=0){
    stats <- filter(stats, W>Wmin)
    return(ggplot(stats, aes(p))
           + geom_histogram(breaks=breaks)
           + geom_hline(yintercept=nrow(stats)/(length(breaks)-1))
           + ggtitle(tag)
    )
}

## rangePlots are named for their order functions:
## slug, blob and milli (for millipede)
## slug is currently preferred
rangePlot <- function(tf, orderFun=slug, alpha=0.05
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

milli <- function(tf){
    numEst <- nrow(tf)
    return(tf
           %>% arrange(est)
           %>% mutate(
               quantile=((1:numEst)-1/2)/numEst
           )
    )
}

blob <- function(tf){
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
               estQ=((1:numEst)-1/2)/numEst
               , pos = estQ*(lower-median(lower))
               +(1-estQ)*(upper-median(upper))
           )
           %>% arrange(pos)
           %>% mutate(
               quantile=((1:numEst)-1/2)/numEst
           )
    )
}