# script to test god's unbiased rarity estimator on simluated community data

#make a lognormal SAD with S species
S<-50
a<-exp(rnorm(S))
true_p=a/sum(a)

#Sample N from it to get observations
N<-300
obs_namelist<-sample(1:S, size=N, replace=T, prob=a)
freqs<-unlist(lapply(1:S, function(x){length(which(obs_namelist==x))}))


#Now make GUE (God's Unbiased Estimator)
GUE<-function(freqs, true_p,l){
    ifelse((l==0),
        exp(sum(freqs*log(1/true_p))/sum(freqs)),
        (sum(freqs*(1/true_p)^l))^(1/l)
    )
}

GUE(true_p, true_p, 1)
GUE(freqs/sum(freqs), true_p, 1)    

GUE(true_p, true_p, 0)
GUE(freqs/sum(freqs), true_p, 0)

GUE(true_p, true_p, -1)
GUE(freqs/sum(freqs), true_p, -1)
