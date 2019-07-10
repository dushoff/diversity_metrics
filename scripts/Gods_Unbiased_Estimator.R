# script to test god's unbiased rarity estimator on simluated community data

#a couple useful diversity estimators

library(tidyverse)
library(mobsim)
library(furrr)
source("scripts/helper_funs/estimation_funs.R")

#Now make GUE (God's Unbiased Estimator)
GUE<-function(freqs, true_p,l){
    freqs=freqs[freqs>0]
    true_p=true_p[true_p>0]
    ifelse((l==0),
           exp(sum(freqs*log(1/true_p))/sum(freqs)),
           (sum(freqs*(1/true_p)^l))^(1/l)
    )
}




######
# hacked method to use Chao's way of augmenting bootstrap assemblage. Not sure why hadn't tried before. 
Chao_rare<-function(x){
    zer=length(x[x==0])
    x = x[x>0]
    n = sum(x)
    f1 = rpois(1, sum(x==1))
    f2 = rpois(1, sum(x==2))
    #Coverage, estimated based on rpois draws for true number of singletons and doubletons, I guess
    C = 1 - f1/n*ifelse(f2>0,(n-1)*f1/((n-1)*f1+2*f2),ifelse(f1>0,(n-1)*(f1-1)/((n-1)*(f1-1)+2),0))
    #use coverage to define a weighting for observed frequencies... this is lambda_hat in Chao et al. 2013 and 2014 appendices explaining this bootstrapping procedure. 
    #coverage deficit=p(next individual is a new species)=proportion of true community absent from sample
    # the denominator is the expected probability of observing all x if x/n=p 
    W = (1-C)/sum(x/n*(1-x/n)^n)
    
    #use that weighting here to get p.new for observed species in x
    p.new = c(x/n*(1-W*(1-x/n)^n), rep(0,zer))
    rchao=1/p.new
    return(p.new)
}

#quickly assess how well the hacked chao works

full<-sim_sad(n_sim=1e7, s_pool=120, sad_type = "poilog", sad_coef=list("cv_abund"=5))
#set up prallelization, not really needed for this size
nc<-parallel::detectCores()-1
plan(strategy=multiprocess, workers=nc)


test_wacky<-future_map_dfr(1:100, function(attempt){
    map_dfr(10^seq(2,5,0.5), function(INDS){
        sam=subsam(full, INDS)
        map_dfr(c(-1,0,1), function(L){
        obs = dfun(sam, L)
        newEst = GUE(sam/sum(sam), Chao_rare(sam),L)
        ChaoEst = Chao_Hill_abu(sam, 1-L)
        return(data.frame(size=INDS, ell=L, obs=obs, newEst=newEst, ChaoEst=ChaoEst))
        })
    })
})


# sam<-subsam(full, 100)
# cr<-Chao_rare(sam)
# god_simp<-GUE(sam, cr, -1)
# god_simp

truth<-map_dfr(c(-1,0,1), function(L){
    truediv=dfun(full, L)
    return(data.frame(truediv=truediv, ell=L))
    
})
test_wacky<-left_join(test_wacky, truth)


pdf("figures/Chao_rarity_estimator.pdf")
test_wacky %>% gather(meth,esti, -c(1,2)) %>%
    ggplot(aes(size, esti, color=meth))+
    geom_point(alpha=0.2)+
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~ell)+
    theme_classic()
dev.off()
# #Chao1, with her own weird thing for no doubletons
# c1<-function(x){sum(x>0)+ifelse(
#     sum(x==2)>0,
#     sum(x==1)^2/(sum(x==2)*2),
#     sum(x==1)*(sum(x==1)-1)/2)}


#Good-Turing 1 chao-type estimator
# GT1<-function(x){c1(x)-(sum(x==2))}

#Unsmoothed Good-Turing rarity estimator; problems with infinitiy

# GT2<-function(x){
#     f_tab<-map_dfr(1:max(x), function(y){return(data.frame(abun=y, f=sum(x==y)))})
#     sum(!is.na(map_dfr(1:max(x), function(z){
#     #1/(r+1/sum(n_i)*n_(r+1)/n_r)
#         return(data.frame(f_tab[z,"f"]*f_tab[z,"abun"]/((z+1)/sum(f_tab$f)*max(f_tab[z+1,"f"],0)/f_tab[z,"f"])))
#     }))/sum(x))
# }
##########################################################################
######### BELOW was for ESA abstract, ignore for now
#########################################################
begin<-1
end<-99

#make some different sads with different distributions, eveness, etc. for functions that can take S, N as arguments
# s_type<-rep(c("powbend", "geom", "lnorm"), 4)
#
# c_list<-list(
#       list(s=1, omega=0.01), list(prob=0.01), list(meanlog=5, sdlog=.5)
#      , list(s=1, omega=0.02), list(prob=0.02), list(meanlog=5, sdlog=3)
#      , list(s=1, omega=0.04), list(prob=0.04), list(meanlog=5, sdlog=5)
#      , list(s=1, omega=0.03), list(prob=0.03), list(meanlog=5, sdlog=1)
#     )
#
#
# nc<-parallel::detectCores()-1
# plan(strategy=multiprocess, workers=nc)
# sz<-round(exp(seq(9,19, 0.5)))
# future_map(begin:end, function(x){
#
#     map(c(30,75,150), function(S){
#
#         out<-map_dfr(sz,function(comsize){
#         #number of individuals in sample
#             #SADs with ~S species
#             map_dfr(1:length(s_type),function(saddef){
#
#                     a<-as.numeric(
#                         # sim_sad(30, 100000, sad_type="powbend", sad_coef=list(s=1, omega=0.01))
#                         sim_sad(
#                             S
#                             , comsize
#                             # , sad_type="powbend"
#                             , sad_type = s_type[[saddef]]
#                             , sad_coef= c_list[[saddef]]
#                             # , sad_coef =list(s=1, omega=0.01)
#                             , fix_s_sim = T
#                             )
#                         )
#                     true_p=a/sum(a)
#
#                         #Sample N from it to get observations
#                     map_dfr(c(100, 200, 300, 500), function(N){
#                         obs_namelist<-sample(1:S, size=N, replace=T, prob=true_p)
#                         freqs<-unlist(lapply(1:S, function(x){length(which(obs_namelist==x))}))
#                         # for each ell value
#                         map_dfr(seq(-1,1,1), function(l){
#                             true<-GUE(true_p, true_p, l)
#                             gods<-GUE(freqs/sum(freqs), true_p, l)
#                             naive<-GUE((freqs/sum(freqs))[freqs/sum(freqs)!=0],(freqs/sum(freqs))[freqs/sum(freqs)!=0],l) # equals sum(freqs>0) for richness
#                             chao<-Chao_Hill_abu(freqs, 1-l)
#                             # simple_good1<-GT1(freqs)
#                             # good_plugin<-GT2(freqs)
#
#
#
#                             return(data.frame(true, gods, naive, chao, l, N,S, comsize, saddef))
#                     })
#                 })
#
#             })
#
#         })
#
#         write.csv(out, file=paste("data/GUEsims", S, "no", x, ".csv", sep=""))
#
#     })
# })


guesims<-map_dfr(dir("data"), function(x){
        if(grepl("GUEsims",x)){
        read_csv(paste("data/", x, sep=""))}
    })



rootms<-function(x){sqrt(sum(((x)^2)/length(x)))}

errs<-guesims %>% group_by(l,S,N,comsize,saddef) %>% mutate(ge=gods-true, ne=naive-true, ce=chao-true) %>% summarize_at(.vars=c("ge", "ne", "ce"), .funs=rootms) %>% gather(est, err,ge, ne, ce)

# s %>% filter(S==30) %>% ggplot(aes(true, chao))+geom_point()

(errs %>% ggplot(aes(N, err+1, color=est, shape=as.factor(comsize)))
    +geom_jitter(width=40, alpha=0.2)
    +theme_classic()
    +facet_wrap(~S+l)
    +scale_y_log10(limits=c(1, 50))

)

plot_ests<-function(df, e1, e2){(df %>% ggplot(aes(x=get(e1), y=get(e2)))
    +geom_point()
    +labs(x=e1, y=e2)
    +geom_hline(yintercept=mean(true), color="red")
    +geom_vline(xintercept=mean(true), color="red")
	+geom_abline()
    +theme_classic()
    +facet_wrap(true))
}



plot_ests(out, "gods", "chao1")
plot_ests(out, "gods", "naive")
plot_ests(out, "chao1", "naive")
plot_ests(out, "gods", "simple_good1")
plot_ests(out, "chao1", "simple_good1")
# plot_ests(out, "gods", "good_plugin")
# plot_ests(out, "good_plugin", "naive")





