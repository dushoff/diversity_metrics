quantile.default <-function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE
             , type = 7, ...){
        if(is.factor(x)) { #worry about non-numeric data
            if(!is.ordered(x) || ! type %in% c(1L, 3L))
                stop("factors are not allowed")
            lx <- levels(x)
        } else lx <- NULL
        if (na.rm){
            x <- x[!is.na(x)]
        } else if (anyNA(x)){
            stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
            }
        eps <- 100*.Machine$double.eps #this is to deal with rounding things sensibly
        if (any((p.ok <- !is.na(probs)) & (probs < -eps | probs > 1+eps)))
            stop("'probs' outside [0,1]")
        
        #####################################
        # here is where terms really used in default type==7 situation get defined
        
        n <- length(x) #how many observations are in sample?
        
        if(na.p <- any(!p.ok)) { # set aside NA & NaN
            o.pr <- probs
            probs <- probs[p.ok]
            probs <- pmax(0, pmin(1, probs)) # allow for slight overshoot
        }
        
        np <- length(probs) #how many quantiles are you computing?
        
        if (n > 0 && np > 0) { #have positive observations and # quantiles to compute
            if(type == 7) { # be completely back-compatible
                
                index <- 1 + (n - 1) * probs #this gives the order statistic of the quantiles
                lo <- floor(index)  #this is the observed order statistic just below each quantile
                hi <- ceiling(index) #above
                x <- sort(x, partial = unique(c(lo, hi))) #the partial thing is to reduce time to sort, 
                #and it only guarantees that sorting is "right" at these order statistics, important for large vectors 
                #ties are not broken and tied elements just stay in their original order
                qs <- x[lo] #the values associated with the "floor" order statistics
                i <- which(index > lo) #which of the order statistics for the quantiles do not land on an order statistic for an observed value
                
                #this is the difference between the order statistic and the available ranks, i think
                h <- (index - lo)[i] # > 0	by construction 
                ##	    qs[i] <- qs[i] + .minus(x[hi[i]], x[lo[i]]) * (index[i] - lo[i])
                ##	    qs[i] <- ifelse(h == 0, qs[i], (1 - h) * qs[i] + h * x[hi[i]])
                qs[i] <- (1 - h) * qs[i] + h * x[hi[i]] # This is the interpolation step: assemble the estimated quantile by removing h*low and adding back in h*high. 
                # h is the arithmetic difference between the desired order statistic amd the available ranks
                #interpolation only occurs if the desired order statistic is not observed, e.g. .5 quantile is the actual observed median if n is odd. 
                # This means having a more extreme 99th observation doesn't matter when computing the .75 quantile
            
                
                ###################################
                # print all of these things
                
                cat("floor pos=", c(lo))
                cat("\nceiling pos=", c(hi))
                cat("\nfloor values= ", c(x[lo]))
                cat( "\nwhich floors not targets? ", c(i))
                cat("\ninterpolate between ", c(x[lo[i]]), ";", c(x[hi[i]]))
                cat( "\nadjustment values= ", c(h))
                cat("\nquantile estimates:")
           
        }else if (type <= 3){## Types 1, 2 and 3 are discontinuous sample qs.
                    nppm <- if (type == 3){ n * probs - .5 # n * probs + m; m = -0.5
                    } else {n * probs} # m = 0
                    
                    j <- floor(nppm)
                    h <- switch(type,
                                (nppm > j),		# type 1
                                ((nppm > j) + 1)/2, # type 2
                                (nppm != j) | ((j %% 2L) == 1L)) # type 3
                
                    } else{
                    ## Types 4 through 9 are continuous sample qs.
                    switch(type - 3,
                           {a <- 0; b <- 1},    # type 4
                           a <- b <- 0.5,   # type 5
                           a <- b <- 0,     # type 6
                           a <- b <- 1,     # type 7 (unused here)
                           a <- b <- 1 / 3, # type 8
                           a <- b <- 3 / 8) # type 9
                    ## need to watch for rounding errors here
                    fuzz <- 4 * .Machine$double.eps
                    nppm <- a + probs * (n + 1 - a - b) # n*probs + m
                    j <- floor(nppm + fuzz) # m = a + probs*(1 - a - b)
                    h <- nppm - j
                    
                    if(any(sml <- abs(h) < fuzz)) h[sml] <- 0
         
                x <- sort(x, partial =
                              unique(c(1, j[j>0L & j<=n], (j+1)[j>0L & j<n], n))
                )
                x <- c(x[1L], x[1L], x, x[n], x[n])
                ## h can be zero or one (types 1 to 3), and infinities matter
                ####        qs <- (1 - h) * x[j + 2] + h * x[j + 3]
                ## also h*x might be invalid ... e.g. Dates and ordered factors
                qs <- x[j+2L]
                qs[h == 1] <- x[j+3L][h == 1]
                other <- (0 < h) & (h < 1)
                if(any(other)) qs[other] <- ((1-h)*x[j+2L] + h*x[j+3L])[other]
        
                } 
        } else {
            qs <- rep(NA_real_, np)}
        
        if(is.character(lx)){
            qs <- factor(qs, levels = seq_along(lx), labels = lx, ordered = TRUE)}
        if(names && np > 0L) {
            names(qs) <- format_perc(probs)
        }
        if(na.p) { # do this more elegantly (?!)
            o.pr[p.ok] <- qs
            names(o.pr) <- rep("", length(o.pr)) # suppress <NA> names
            names(o.pr)[p.ok] <- names(qs)
            o.pr
        } else qs
}

####################

# fake data
x<-c(1,2,2,2,3,3,3,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,7,99)
y<-c(1,2,2,2,3,3,3,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,7,9)
z<-c(1,2,2,2,3,3,3,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,7)

#quantiles "of interest"
probs<-c(0.5, 0.75, 0.95, 0.975)

# a tiny bit of illustrative behavior
quantile.default(x,probs=probs, names=F)
quantile.default(y,probs=probs, names=F) #only difference is .975 quantile since that is driven by highest 2 observations
quantile.default(z,probs=probs, names=F) # This shifts everything b/c now none of the quantiles fall on an observation (and of course the distribution changed...)... but 
#.75 quantile is stil 5.0 b/c the observations just above and below the order statistic for that quantile are still 5. However, it got there for a different reason.

#how does rescaling affect quantile estimates?
sqrt(quantile.default(x^2, probs=probs, names=F))
exp(quantile.default(log(x), probs=probs, names=F))
