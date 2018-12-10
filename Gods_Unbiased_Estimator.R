# script to test god's unbiased rarity estimator on simluated community data

#a couple useful diversity estimators

library(tidyverse)

#Now make GUE (God's Unbiased Estimator)
GUE<-function(freqs, true_p,l){
    ifelse((l==0),
           exp(sum(freqs*log(1/true_p))/sum(freqs)),
           (sum(freqs*(1/true_p)^l))^(1/l)
    )
}


#Chao1, with her own weird thing for no doubletons
c1<-function(x){sum(x>0)+ifelse(
    sum(x==2)>0,
    sum(x==1)^2/(sum(x==2)*2),
    sum(x==1)*(sum(x==1)-1)/2)}

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
reps<-50
#number of true species
S<-75
#number of individuals in sample
N<-300
l<-1

out<-map_dfr(1:reps, function(x){
#make a lognormal SAD with S species

a<-exp(rnorm(S))
true_p=a/sum(a)

#Sample N from it to get observations

obs_namelist<-sample(1:S, size=N, replace=T, prob=a)
freqs<-unlist(lapply(1:S, function(x){length(which(obs_namelist==x))}))


true<-GUE(true_p, true_p, l)
gods<-GUE(freqs/sum(freqs), true_p, l)   
naive<-GUE((freqs/sum(freqs))[freqs/sum(freqs)!=0],(freqs/sum(freqs))[freqs/sum(freqs)!=0],l) # equals sum(freqs>0) for richness
chao1<-c1(freqs)
simple_good1<-GT1(freqs)
# good_plugin<-GT2(freqs)



return(data.frame(true, gods, naive, chao1, simple_good1, l, N))
})


plot_ests<-function(df, e1, e2){(df %>% ggplot(aes(x=get(e1), y=get(e2)))
    +geom_point()
    +labs(x=e1, y=e2)
    +geom_hline(yintercept=S, color="red")
    +geom_vline(xintercept=S, color="red")
	 +geom_abline()
    +theme_classic())
}

plot_ests(out, "gods", "chao1")
plot_ests(out, "gods", "naive")
plot_ests(out, "chao1", "naive")
plot_ests(out, "gods", "simple_good1")
plot_ests(out, "chao1", "simple_good1")
# plot_ests(out, "gods", "good_plugin")
# plot_ests(out, "good_plugin", "naive")





