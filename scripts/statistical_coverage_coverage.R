#check statistical coverage for CI for coverage-standardized samples

library(furrr)
source("scripts/helper_funs/uniroot_gamma_and_lnorm.R")
R_FUTURE_FORK_ENABLE=T
library(iNEXT)
# simulate full community
fullcomm<-fit_SAD(rich=400, simpson=60, dstr="gamma")
fullcomm2<-fit_SAD(rich=200, simpson=60, dstr="lnorm")
hist(fullcomm2[[3]])

plot(200-min_rank(fullcomm2[[3]]), fullcomm2[[3]] )
#get each sample size "reos" times
nc<-7
plan(strategy = multiprocess, workers = nc)

reps<-50
SS<-200
map(seq(100,1500, 100), function(SS){
somedata_to_think_about<-future_map_dfr(1:reps, function(rep){
    map_dfr(1:100, function(myrep){
        c1<-sample_infinite(fullcomm[[3]], 4*SS)
        c2<-sample_infinite(fullcomm2[[3]], SS)
        TC2<-sum((c2>1)*fullcomm2[[3]])
        asyc1<-Chao_Hill_abu(c1, c(0,1,2))
        asyc2<-Chao_Hill_abu(c2, c(0,1,2))
        oc1<-unlist(lapply(c(1,0,-1), function(l){dfun(c1, l)}))
        oc2<-unlist(lapply(c(1,0,-1), function(l){dfun(c2, l)}))
        
        covdat<-estimateD(list("c1"=c1,"c2"=c2), base="coverage", level=NULL, conf=NULL)
        data.frame(SS=SS, cbind(rbind(asyc1, asyc2), rbind(oc1, oc2), covdat), TC2)
                   # , c1Rich=asyc1[1],c2Rich=asyc2[1], c1Shan=asyc1[2], c2Shan=asyc2[2],c1Simp=asyc1[3], c2Simp=asyc2[3])
    })
})

# mydf<-read.csv("data/fromR/data_to_think_about.csv")
# mydf<-read.csv("data/fromR/data_to_think.csv")
mydf<-somedata_to_think_about
mydf1<-mydf %>% filter(site=="c1") 
mydf2<-mydf %>% filter(site=="c2") 
c2*fullcomm2[[3]]
comp<-(mydf1 %>%mutate_all(as.numeric))/(mutate_all(mydf2, as.numeric))

print(comp %>% summarize_all(mean))
})
mydf2 %>% summarize(meanm=mean(m),sdm=sd(m))


pdf(file="figures/chao1_ratio_goofy.pdf")
mydf %>% filter(method=="observed" &site=="c2") %>%ggplot(aes(SC))+geom_histogram(alpha=0.2, fill="red")+
    geom_histogram(aes(`X2.1`/`X1`), alpha=0.2, fill="blue")+
    geom_histogram(aes(TC2), alpha=0.2, fill="darkblue")+
    geom_histogram(aes((`X2.1`)/200), fill="darkred",alpha=0.2)+
    geom_text(x=0.2, y=1000, label="true proportion species", color="darkred")+
    geom_text(x=0.8, y=1000, label="obs/Chao1 proportion", color="red")+
    geom_text(x=0.5, y=700, label="estimated coverage", color="blue")+
    geom_text(x=0.6, y=800, label="true coverage", color="darkblue")+theme_classic()+labs(x="")+
    xlim(c(0,1))
dev.off()

mydf
mydf %>% group_by(SS, order) %>% summarize(expected=mean(qD), vard=sd(qD), iftvard= (sum(qD.UCL-qD.LCL)/(5000*1.96))
                                           , toolo=sum(qD.LCL>=expected)/n()
                                           , toohi=sum(qD.UCL<=expected)/n(), isright=1-sum(toohi, toolo)
                                           , varn=sd(m), meann=(mean(m)))
