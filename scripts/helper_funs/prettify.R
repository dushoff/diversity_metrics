################################
# function to make pretty numbers evenly spaced across axis that has undergone scale transformation
# round numbers, more aggressively the larger they are
prettify <- function(breaks){
    digits <- -floor(log10(abs(breaks))) + 1
    digits[breaks == 0] <- 0
    return((round(breaks, digits = digits))[round(breaks, digits = digits)!=(-Inf)]) #klugey fix to -Inf
}
