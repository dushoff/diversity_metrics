# script to test god's unbiased rarity estimator on simluated community data
library(tidyverse)
#a couple useful diversity estimators

#Now make GUE (God's Unbiased Estimator)
GUE<-function(freqs, true_p,l){
    ifelse((l==0),
           exp(sum(freqs*log(1/true_p))/sum(freqs)),
           (sum(freqs*(1/true_p)^l))^(1/l)
    )
}
#Chao1, with her own weird thing for no doubletons
c1<-function(x){length(x>0)+ifelse(
    sum(x==2)>0,
    sum(x==1)^2/(sum(x==2)*2),
    sum(x==1)*(sum(x==1)-1)/2)}

#do reps times to test bias, store to a list
reps<-50
#number of true species
S<-50
#number of individuals in sample
N<-30
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
naive<-GUE((freqs/sum(freqs))[freqs/sum(freqs)!=0],(freqs/sum(freqs))[freqs/sum(freqs)!=0],l)
chao1<-c1(freqs)



return(data.frame(true, gods, naive, chao1, S, l, N))
})


plot_ests<-function(df, e1, e2){(df %>% ggplot(aes(x=get(e1), y=get(e2)))
    +geom_point()
    +labs(x=e1, y=e2)
    +geom_hline(yintercept=S, color="red")
    +geom_vline(xintercept=S, color="red")
    +theme_classic())
}

plot_ests(out, "gods", "chao1")
plot_ests(out, "gods", "naive")
plot_ests(out, "chao1", "naive")



