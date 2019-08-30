# Generates a semi-parametric SAD based on richness, and simpson, and total abundance. Requires a bit of thought b/c function requires a "prior" for the "shape" parameter of either "gamma" or "lnorm." For gamma, seems like if it tries the a shape parameter too close to 0, it returns garbage in the form of ur_gamma(0)==-Inf.

#returns a list. First element is SAD type and shape parameter. Second element is summary community info (total abundance, richness, shannon, simpson). Final element is abundance vector. ) abundances throw warning but not error. 

source("scripts/helper_funs/estimation_funs.R")

#define variables but also give them values for messing with
# totAb<-1e7 #total true abundance in community
# rich<-50 # true richness
# simpson<-40 #true simpson


#define my gamma distribution
discrete_gamma<-function(rich, totAb, x){
    round(qgamma(seq((1/rich)/2, 1-(1/rich)/2, (1/rich)), shape=x, scale=(totAb/rich)/x))
    }

#function for uniroot to optimize, according to simpsons
ur_gamma<-function(x,rich=rich, simpson=simpson, totAb=totAb,...){simpson-dfun(discrete_gamma(rich, totAb,x), -1)}

# gamShape<-uniroot(ur_gamma, lower=1e-2, upper=1e2)
# gamShape

# ur_gamma(1e-5)

#define my lognormal distribution
discrete_lnorm<-function(rich, totAb, x){
    round(qlnorm(seq((1/rich)/2, 1-(1/rich)/2, (1/rich)), meanlog=log((totAb/rich)^2/sqrt(x^2+(totAb/rich)^2)), sdlog=(log(1+x^2/(totAb/rich)^2))^(1/2)))
    }

#function for uniroot to optimize, according to simpsons
ur_lognorm<-function(x, rich=rich, simpson=simpson, totAb=totAb,...){ ddiff=simpson-dfun(discrete_lnorm(rich, totAb, x), -1)
    return(ddiff)} #here x is arithmetic sd of lognormal.

fit_SAD<-function(totAb=1e7, rich=50, simpson=40, int_lwr=0,int_uppr=1e9, dstr="lnorm"){
    #check feasibility
    if(simpson>rich){return("ERROR: Hill-Simpson diversity cannot be greater than richness")}
    
    #check dstr makes sense
    ifelse( !(dstr %in% c("lnorm", "gamma")), return("ERROR: dstr must be either `lnorm` or `gamma`"),
            
        #generate SAD when dstr=lnorm 
        {if(dstr=="lnorm"){
            #uses an optimizer called uniroot to find x when ur_lognorm(x)==0
                fit_par=tryCatch(uniroot(function(x){ur_lognorm(simpson=simpson, rich=rich, totAb=totAb,x)}, lower=int_lwr, upper=int_uppr), error=function(e) message("ERROR: test int_lwr and int_uppr in ur_ function, output must have opposite signs"))
                
                #make sure to return abundances!
                abus=tryCatch(discrete_lnorm(rich, totAb, fit_par$root), error=function(e) {message("did not fit param")
                    return(rep(100, length(rich)))}
                )
                
        }
            
            #generate SAD when dstr="gamma"
        if(dstr=="gamma"){
            
            #uses an optimizer called uniroot to find x when ur_gamma(x)==0
            fit_par=tryCatch(uniroot(function(x){ur_gamma(simpson=simpson, rich=rich, totAb=totAb, x)}, lower=int_lwr, upper=int_uppr), error=function(e) message("ERROR: test int_lwr and int_uppr in ur_ function, output must have opposite signs"))
            
            #make sure to return abundances!
            abus=tryCatch(discrete_gamma(rich, totAb, fit_par$root), error=function(e) {
                message("did not fit param")
                return(rep(100, length(rich)))
                }
            )
        }
        
            #return Hill-Shannon also
        shannon=dfun(abus, 0)
        if(sum(abus==0)>0) print("WARNING: you simulated species with 0 abundance")
        
        return(list("distribution_info"=c("distribution"=dstr, "fitted parameter"=fit_par$root)
                    , "community_info"=c("richness"=rich, "Hill-Shannon"=shannon, "Hill-Simpson"=simpson, "total abundance"=totAb)
                    , "abundances"=abus
                    
                   ))
        }
    )
}

#test function
fit_SAD(dstr="lnorm") #works
fit_SAD(dstr="nonsense")  #gives custom error (though not as error message)
fit_SAD(dstr="gamma") #gives custom error
fit_SAD(dstr="gamma", int_lwr=1e-2)  #works
fit_SAD(dstr="lnorm", rich=50, simpson=90) #gives custom error (though not as error message)
fit_SAD(dstr="lnorm", rich=50, simpson=2) #works! gives custom warning (though not as a waring message)



