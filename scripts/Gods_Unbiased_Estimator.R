# script to test god's unbiased rarity estimator on simluated community data

#a couple useful diversity estimators

library(tidyverse)
library(mobsim)
library(furrr)
source("scripts/helper_funs/estimation_funs.R")

#Now make GUE (God's Unbiased Estimator)
GUE<-function(freqs, true_p,l){
    ifelse((l==0),
           exp(sum(freqs*log(1/true_p))/sum(freqs)),
           (sum(freqs*(1/true_p)^l))^(1/l)
    )
}


# #Chao1, with her own weird thing for no doubletons
# c1<-function(x){sum(x>0)+ifelse(
#     sum(x==2)>0,
#     sum(x==1)^2/(sum(x==2)*2),
#     sum(x==1)*(sum(x==1)-1)/2)}


#Good-Turing 1 chao-type estimator
GT1<-function(x){c1(x)-(sum(x==2))}

#Unsmoothed Good-Turing rarity estimator; problems with infinitiy

# GT2<-function(x){
#     f_tab<-map_dfr(1:max(x), function(y){return(data.frame(abun=y, f=sum(x==y)))})
#     sum(!is.na(map_dfr(1:max(x), function(z){
#     #1/(r+1/sum(n_i)*n_(r+1)/n_r)
#         return(data.frame(f_tab[z,"f"]*f_tab[z,"abun"]/((z+1)/sum(f_tab$f)*max(f_tab[z+1,"f"],0)/f_tab[z,"f"])))
#     }))/sum(x))
# }

#do reps times to test bias, store to a list
reps<-99

 
# sim_sad(30, 100000, sad_type="gamma", sad_coef=list(shape=3,scale=3))
# sim_sad(30, 100000, sad_type="powbend", sad_coef=list(s=1, omega=0.01))
# sim_sad(30, 100000, sad_type="powbend", sad_coef=list(s=1, omega=0.04))
# sim_sad(30, 100000, sad_type="geom", sad_coef=list(prob=.04))
# sim_sad(s_pool=NULL,n_sim=100000, sad_type="ls", sad_coef=list(alpha=4, N=100000), fix_s_sim=30)
# sim_sad(s_pool = 30, n_sim = 10000, sad_type = "poilog",
#         sad_coef = list("mu" = 5, "sig" = 0.5))
# sim_sad(s_pool = 30, n_sim = 10000, sad_type = "lnorm",
#         sad_coef = list("meanlog" = 5, "sdlog" = 0.5))
# sim_sad(s_pool = 30, n_sim = 10000, sad_type = "lnorm",
#         sad_coef = list("meanlog" = 5, "sdlog" = 3))


#make some different sads with different distributions, eveness, etc. for functions that can take S, N as arguments
s_type<-rep(c("powbend", "geom", "lnorm", "poilog"), 3)
c_list<-list(list(s=1, omega=0.01), list(prob=0.01),list(meanlog=5, sdlog=0.5), list(mu=5, sig=0.5),list(s=1, omega=0.02), list(prob=0.02),list(meanlog=5, sdlog=3), list(mu=5, sig=3), list(s=1, omega=0.04), list(prob=0.04),list(meanlog=5, sdlog=5), list(mu=5, sig=5))


nc<-parallel::detectCores()-1
plan(strategy=multiprocess, workers=nc)
#number of true species
s<-map_dfr(1:reps, function(x){
    map_dfr(c(30,75,150), function(S){
        future_map_dfr(round(exp(seq(12,19, 0.5))),function(comsize){
        #number of individuals in sample
            #SADs with ~S species
            map_dfr(1:length(s_type),function(saddef){
                    a<-as.numeric(
                        # sim_sad(30, 100000, sad_type="powbend", sad_coef=list(s=1, omega=0.01))
                        sim_sad(
                            S
                            , comsize
                            # , sad_type="powbend"
                            , sad_type = s_type[[saddef]]
                            , sad_coef= c_list[[saddef]]
                            # , sad_coef =list(s=1, omega=0.01)
                            , fix_s_sim = T
                            )
                        )
                    true_p=a/sum(a)
                    
                        #Sample N from it to get observations
                    map_dfr(c(100, 200, 300), function(N){
                        obs_namelist<-sample(1:S, size=N, replace=T, prob=true_p)
                        freqs<-unlist(lapply(1:S, function(x){length(which(obs_namelist==x))}))
                        # for each ell value
                        map_dfr(seq(-1,1,1), function(l){
                            true<-GUE(true_p, true_p, l)
                            gods<-GUE(freqs/sum(freqs), true_p, l)   
                            naive<-GUE((freqs/sum(freqs))[freqs/sum(freqs)!=0],(freqs/sum(freqs))[freqs/sum(freqs)!=0],l) # equals sum(freqs>0) for richness
                            chao<-Chao_Hill_abu(freqs, 1-l)
                            # simple_good1<-GT1(freqs)
                            # good_plugin<-GT2(freqs)
                            
                            
                            
                            return(data.frame(true, gods, naive, chao, l, N,S, comsize, saddef))
                    })
                })
            })
        })
    })
})

rootms<-function(x){sqrt(sum(((x)^2)/length(x)))}

errs<-s %>% group_by(l,S,N,saddef) %>% mutate(ge=gods-true, ne=naive-true, ce=chao-true) %>% summarize_at(.vars=c("ge", "ne", "ce"), .funs=rootms) %>% gather(est, err,ge, ne, ce)

s %>% filter(S==30) %>% ggplot(aes(true, chao))+geom_point()

(errs %>% ggplot(aes(N, err, color=est, shape=saddef))
    +geom_jitter(width=12, alpha=0.4, size=3)
    +theme_classic()
    +facet_wrap(~S+l)
    
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





