library(ggplot2); theme_set(theme_bw())

set.seed(605)

print(ssrDiversity(powPop(5, 0.5)))
print(piePop(powPop(5, 0.5)))

print(ssrDiversity(powPop(10, 1.5)))
print(piePop(powPop(10, 1.5)))

print(ssrDiversity(powPop(20, 3)))
print(piePop(powPop(20, 3)))
