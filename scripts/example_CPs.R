# I am sure Jonathan has this but example checkplots for parametric distributions
library(tidyverse)
library(furrr)
library(gridExtra)

nc<-7#per Rob's recommendation


plan(strategy=multiprocess, workers=nc) #this is telling the computer to get ready for the future_ commands

# Normal, 1-sample t-test, single sample size

true_mu<-11
reps<-5e4
ss<-100
set.seed(42)
sampledat<-map_dfr(1:reps, function(x){
    data.frame(myrep=x, sample_vals=rnorm(100, mean=true_mu, sd=4))
})


ttest_ps<-future_map_dfr(1:reps/100, function(x){
    data.frame(rep=x, p=t.test(sampledat[sampledat$myrep==x, "sample_vals"], mu=true_mu, alternative="greater")$p.value)
})


tt<-ttest_ps %>% 
    ggplot(aes(p))+
    geom_histogram(breaks=seq(0, 1, by=0.05))+
    theme_classic()+
    labs(x="p values; Student's 1 sample t-test")+
    ylim(c(0,10000))


# some binomial tests

true_p=0.75

sample_bin<-rbinom(reps, ss, true_p)

binom_default_ps<-future_map_dfr(1:reps, function(x){
    data.frame(rep=x, default_p=binom.test(sample_bin[x], ss, true_p, alternative="greater")$p.value
               , normal_approx=prop.test(sample_bin[x], ss, true_p, alternative="greater")$p.value)
})


b1<-binom_default_ps %>% 
    ggplot(aes(default_p))+
    geom_histogram(breaks=seq(0, 1, by=0.05))+
    theme_classic()+
    labs(x="p-values; exact binomial test")+
    ylim(c(0,10000))


b2<-binom_default_ps %>% 
    ggplot(aes(normal_approx))+
    geom_histogram(breaks=seq(0, 1, by=0.05))+
    theme_classic()+
    labs(x="p-values; normal approximation binomial test")+
    ylim(c(0,10000))

pdf("figures/parametric_checkplot_examples.pdf")
grid.arrange(tt, b1, b2)
dev.off()


               