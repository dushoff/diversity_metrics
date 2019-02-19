library(RColorBrewer)
library(ggplot2); theme_set(theme_bw())

set.seed(605)

print(pieStats(powPop(5, 1.26)))
print(pieStats(powPop(5,0.4)))

print(pieStats(powPop(12, 1.26)))
print(pieStats(powPop(12,2.2)))

print(pieStats(powPop(20, 2.2)))
print(pieStats(powPop(20,4)))

print(pieStats(powPop(20,1.26)))
print(pieStats(powPop(20,.4)))
powPop

