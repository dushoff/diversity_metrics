# play with uniroot and figure out how to generate SADS based on Richness and Simpson
source("scripts/helper_funs/estimation_funs.R")

#define variables but also give them values for messing with
totAb<-1e7 #total true abundance in community
rich<-50 # true richness
simpson<-40 #true simpson


#define my gamma distribution
discrete_gamma<-function(rich, totAb, x){
    qgamma(seq((1/rich)/2, 1-(1/rich)/2, (1/rich)), shape=x, scale=(totAb/rich)/x)
    }

#function for uniroot to optimize, according to simpsons
ur_gamma<-function(x,...){simpson-dfun(discrete_gamma(rich, totAb,x), -1)}

# gamShape<-uniroot(ur_gamma, lower=1e-2, upper=1e2)
# gamShape

# ur_gamma(1e-5)

#define my lognormal distribution
discrete_lnorm<-function(rich, totAb, x){
    qlnorm(seq((1/rich)/2, 1-(1/rich)/2, (1/rich)), meanlog=log((totAb/rich)^2/sqrt(x^2+(totAb/rich)^2)), sdlog=(log(1+x^2/(totAb/rich)^2))^(1/2))
    }

ur_lognorm<-function(x,...){ ddiff=simpson-dfun(discrete_lnorm(rich, totAb, x), -1)
    return(ddiff)} #here x is arithmetic sd of lognormal.

fit_SAD<-function(totAb=1e7, rich=50, simpson=40, int_lwr=0,int_uppr=1e9, dstr="lnorm"){
    myerr<-NULL
    if(dstr=="lnorm"){
            fit_par=tryCatch(uniroot(ur_lognorm, lower=int_lwr, upper=int_uppr), error=function(e) message("ERROR: test int_lwr and int_uppr in ur_ function, output must have opposite signs"))
            abus=tryCatch(discrete_lnorm(rich, totAb, fit_par$root), error=function(e) {message("did not fit param")
                return(rep(100, length(rich)))}
            )
            
        }
   if(dstr=="gamma"){
        fit_par=tryCatch(uniroot(ur_gamma, lower=int_lwr, upper=int_uppr), error=function(e) message("ERROR: test int_lwr and int_uppr in ur_ function, output must have opposite signs"))
        abus=tryCatch(discrete_gamma(rich, totAb, fit_par$root), error=function(e) {message("did not fit param")
                      return(rep(100, length(rich)))}
                      )
   }

        
    shannon=dfun(abus, 0)
    return(list("distribution_info"=c("distribution"=dstr, "fitted parameter"=fit_par$root)
                , "abundances"=abus
                , "diversities"=c("richness"=rich, "Shannon-Hill"=shannon, "Simpson-Hill"=simpson)
               ))
}

fit_SAD(dstr="lnorm", int_uppr=0.0001)    
        
