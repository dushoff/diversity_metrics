#checkplot_inf is the machinery to do checkplots for asymptotic diversity estimates

# it needs checkchao from scripts/helper_funs/estimation_funs.R 
# and to work well, furrr loaded with a plan for parallel


checkplot_inf<-function(SAD, B=2000, l, inds, reps){
    hillname<-ifelse(l==-1, "Hill-Simpson", ifelse(l==0, "Hill-Shannon", "richness"))
    td<-SAD$community_info[hillname] #grab true diversity from SAD object
  
    future_map_dfr(1:reps,function(x){
        
        
        obs <- sample_infinite(SAD$rel_abundances, size=inds) #subsample the whole community with # individuals=size
        
        chaotile <- checkchao(x=obs, B=B, l=l, truediv=td) #then do B bootstrap samples for the augmented community based on that sample
        return(myout=data.frame(p=chaotile$p
                                , truediv=chaotile$truediv
                                , chaoest=chaotile$chaoest
                                , obsD=chaotile$obsD
                                , upper=chaotile$upper
                                , lower=chaotile$lower
                                , l
                                , inds
                                , reps)
        )
        
    })
}