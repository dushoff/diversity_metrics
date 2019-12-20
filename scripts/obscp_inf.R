#obscp_inf for making checkplots of sample diveristy
obscp_inf <- function(l=l, size=size, SAD=SAD, B=2000, truemun=truemun, conf=0.95,...){
    sam <- sample_infinite(SAD$rel_abundances, size=size)
    data.bt = rmultinom(B,size,Bt_prob_abu(sam)) #this genenerates "bootstrapped" samples, using Chao's fancy method
    obs<-dfun(sam,l) #observed diversity
    pro = apply(data.bt,2,function(boot)dfun(boot, l)) #sample diversity for bootstraps
    pro_mc<-pro-mean(pro)+obs
    
    less<-sum(pro_mc<truemun)/length(pro_mc)
    more<-(length(pro_mc)-sum(pro_mc>truemun))/length(pro_mc)
    p<-runif(1, min(less, more), max(less, more))
    
    lower<-max(pro_mc[which(min_rank(pro_mc)<=max(floor(B*(1-conf)/2),1))])
    upper<-min(pro_mc[which(min_rank(-pro_mc)<=max(floor(B*(1-conf)/2),1))])
    
    less_no_mc<-sum(pro<truemun)/length(pro)
    more_no_mc<-(length(pro)-sum(pro>truemun))/length(pro)
    p_no_mc<-runif(1, min(less, more), max(less, more))
    
    lower_no_mc<-max(pro[which(min_rank(pro)<=max(floor(B*(1-conf)/2),1))])
    upper_no_mc<-min(pro[which(min_rank(-pro)<=max(floor(B*(1-conf)/2),1))])
    
    
    # chaotile_mc<-findInterval(truemun, quantile(pro_mc, seq(0,1,0.0005)), all.inside = T)/20
    # chaotile<-findInterval(truemun, quantile(pro, seq(0,1,0.0005)), all.inside=T)/20
    
    return(data.frame("p"=p, "p_no_mc"=p_no_mc
                      , upper = upper
                      , lower = lower
                      , upper_no_mc = upper_no_mc
                      , lower_no_mc = lower_no_mc
                      , "truemu"=truemun
                      , "obsD"=obs
                      , "l"=l
                      , "size"=size ))
    
}

#try checking obs uses obscp_inf
trycheckingobs<-function(SAD, size){
    
   
        truemun<-truemu_inf(SAD$rel_abundances, size=size, reps=reps, l=ell)
        future_map_dfr(1:reps, function(reps){obscp_inf(l=ell, size, SAD, truemun=truemun, B=Bnum)
            
            
    
    })
}
