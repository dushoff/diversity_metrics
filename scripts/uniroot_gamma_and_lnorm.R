# play with uniroot and figure out how to generate SADS based on Richness and Simpson
source("scripts/helper_funs/estimation_funs.R")


totAb<-1e7
rich<-100
simpson<-90
simp<-1

ur_gamma<-function(x){simpson-dfun(qgamma(seq((1/rich)/2, 1-(1/rich)/2, (1/rich)), shape=x, scale=(totAb/rich)/x), -1)}

gamShape<-uniroot(ur_gamma, lower=1e-5, upper=1e2)


ur_gamma(1e-5)


ur_lognorm<-function(x){ ddiff=simpson-dfun(qlnorm(seq((1/rich)/2, 1-(1/rich)/2, (1/rich)), meanlog=log((totAb/rich)^2/sqrt(x^2+(totAb/rich)^2)), sdlog=(log(1+x^2/(totAb/rich)^2))^(1/2)), -1)
return(ddiff)} #here x is arithmetic sd of lognormal.

sd_arr<-uniroot(ur_lognorm, lower=0, upper=1e9)

