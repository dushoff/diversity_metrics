# checkplot initials
#############
#load functions and packages
library(furrr)#parallelization
source("/home/mr984/scripts/helper_funs/uniroot_gamma_and_lnorm.R")
# source("/Rstudio_Git/Dushoff_diversity/scripts/helper_funs/radplot.R")
# source("scripts/helper_funs/prettify.R")
library(scales)#trans_breaks
# library(cowplot) #sometimes nice stuff for multipanel ggplotting
# invlogit<-arm::invlogit
#library(vegan) # for fisherfit
#library(MASS) # for fitdistr
# library(scales) #for function muted
select<-dplyr::select

# function to remove subscripts
remsub <- function(variable, value){
    return(gsub("_"," ",value))}

#function to summarize frequencies
asab<-function(namevec){as.numeric(table(namevec))}

###################
# function to compute CV
mycv<-function(x){sd(x)/mean(x)}



########################################
# simulate communities with fit_SAD, basically isntantaneous

SADs_list<-map(c("lnorm", "gamma"), function(distr){
    map(c(100, 200), function(rich){
        # map(c(0.05, .15,.25,.5,.75,.85), function(simp_Prop){
            
        map(c(0.01,0.05, .15,.25,.5,.75,.85), function(simp_Prop){ #this is the special sads loop
            fit_SAD(rich = rich, simpson = simp_Prop*(rich-1)+1, dstr = distr)
        })
    })
})

