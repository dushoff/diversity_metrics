# 4)  for each SAD, bin observations, say 100 at a time, by true coverage. This means that in each bin you'd have 100 samples of very similar true coverage, they may have a variety of different sample sizes.
# 
# 5) For each bin, make a boxplot or similar for each Hill number. Goal is to visualize mean diversity given coverage, and the distribution. Should be clean-looking. Record mean diversity, mean coverage for each bin, hill number.
# 
# 6)   Grab samples of say 5e1, 1e2, 5e2, 1e3, 5e3, and for each one compute Chao's estimated diversities (all 3) for a bunch of sample coverages (5 values? 10 values?) where we have narrow bins. Record for each
# 
# 
#     a) if target sample coverage is above or below true sample coverage for that sample
#     b) for each Hill diversity, if expected diversity for that coverage is in, below, or above Chao's interval
# c) sample stats like actual sample size, diversity, coverage
# 
# 7) Find a fun way to plot the upshot: whether statistical coverage of 95%, whether misses are 2.5% high and 2.5% low, how this depends on sample size, coverage, and Hill number.
# 
# 8) Bonus, but might want to do this earlier and with a simple small dataset: biplot of true coverage and Chao's estimated coverage. Lost track of data from the other day and so going to backburner this for now and get all of this up and running on big computer asap.
# 

bs<-fread("data/comm_samp.csv")





choaex
somedata_to_think_about<-map_dfr(SS, function(ss){
    future_map_dfr(1:reps, function(rep){
        map_dfr(1:100, function(myrep){
            #draw a finite sample of size=SS
            # c1<-sample_infinite(fullcomm[[3]], SS)
            c2<-sample_infinite(fullcomm2[[3]], SS)
            # compute actual sample coverage, i.e. summed relative abundance of species represended in sample
            # TC1<-sum((c1>0)*fullcomm[[3]])
            TC2<-sum((c2>0)*fullcomm2[[3]])
            # compute asymptotic estimators
            asyc1<-Chao_Hill_abu(c1, c(0,1,2))
            asyc2<-Chao_Hill_abu(c2, c(0,1,2))
            # compute sample diversities
            oc1<-unlist(lapply(c(1,0,-1), function(l){dfun(c1, l)}))
            oc2<-unlist(lapply(c(1,0,-1), function(l){dfun(c2, l)}))
            
            covdat<-estimateD(list("c1"=c1,"c2"=c2), base="coverage", level=NULL, conf=.95, )
            data.frame(SS=SS
                       , cbind(rbind(asyc1, asyc2)
                               , rbind(oc1, oc2)
                               , covdat)
                       , TC1, TC2
                       , c1Rich=asyc1[1],c2Rich=asyc2[1], c1Shan=asyc1[2]
                       , c2Shan=asyc2[2],c1Simp=asyc1[3], c2Simp=asyc2[3])
        })
    })
})

gammas<-somedata_to_think_about %>% filter(site=="c2" &SS==m)
gammas %>% group_by(SS) %>%  summarize(mean(TC2))

pdf("figures/data_to_think_plot.pdf")
future_map(seq(100,1500, 200), function(sz){
    map(c(0,1,2), function(or){
        gammas %>% 
            filter(m==sz, order==or) %>% 
            mutate(upp=max(.[which(abs(.$SC-.$TC2)<0.00001), "qD.UCL"])
                   ,low=min(.[which(abs(.$SC-.$TC2)<0.00001), "qD.LCL"]) ) %>% 
            ggplot(
                aes(TC2, qD, color=SC-TC2))+
            geom_hline(aes(yintercept = upp))+
            geom_hline(aes(yintercept = low))+
            geom_pointrange(aes(ymax=qD.UCL, ymin=qD.LCL), alpha=0.2, size=0.05)+
            scale_color_gradient2(mid="lightgrey")+
            
            ggtitle(paste0("Chao group coverage rarefaction; ", sz
                           , " individuals, l = ", 1-or
                           ,"\nhorizontal lines are confidence limits when estimated coverage = true coverage"))+
            labs(x="true coverage", y="estimated diversity"
                 , color ="predicted-true coverage")+
            theme_classic()
    })
})
dev.off()

cov_point7<-map_dfr(seq(100,1500, 200), function(SS){
    future_map_dfr(1:reps, function(rep){
        map_dfr(1:100, function(myrep){
            #draw a finite sample of size=SS
            c1<-sample_infinite(fullcomm[[3]], SS)
            c2<-sample_infinite(fullcomm2[[3]], SS)
           
            #get inext for coverage = 0.7
            covdat<-estimateD(list("c1"=c1,"c2"=c2), base="coverage", level=NULL, conf=.95, )
            
            # subsample the observation to the suggested m
            c1s<-subsam(c1, size=covdat$m[1])
            c2s<-subsam(c1, size=covdat$m[2])
            
             # compute actual sample coverage, i.e. summed relative abundance of species represended in sample
           
            TC1<-sum((c1>0)*fullcomm[[3]])
            TC2<-sum((c2>0)*fullcomm2[[3]])
            # # compute asymptotic estimators
            # asyc1<-Chao_Hill_abu(c1, c(0,1,2))
            # asyc2<-Chao_Hill_abu(c2, c(0,1,2))
            # # compute sample diversities
            oc1<-unlist(lapply(c(1,0,-1), function(l){dfun(c1, l)}))
            oc2<-unlist(lapply(c(1,0,-1), function(l){dfun(c2, l)}))
            
            
            data.frame(SS=SS
                       , cbind(rbind(asyc1, asyc2)
                               , rbind(oc1, oc2)
                               , covdat)
                       , TC1, TC2
                       , c1Rich=asyc1[1],c2Rich=asyc2[1], c1Shan=asyc1[2]
                       , c2Shan=asyc2[2],c1Simp=asyc1[3], c2Simp=asyc2[3])
        })
    })
})


# mydf<-read.csv("data/fromR/data_to_think_about.csv")
# mydf<-read.csv("data/fromR/data_to_think.csv")
mydf<-somedata_to_think_about
mydf1<-mydf %>% filter(site=="c1") 
mydf2<-mydf %>% filter(site=="c2") 
# c2*fullcomm2[[3]]
comp<-(mydf1 %>%mutate_all(as.numeric))/(mutate_all(mydf2, as.numeric))

print(comp %>% summarize_all(mean))
})
mydf2 %>% summarize(meanm=mean(m),sdm=sd(m))


pdf(file="figures/chao1_ratio_goofy.pdf")
mydf %>% 
    filter(method=="observed" &site=="c2") %>%
    ggplot(aes(SC))+geom_histogram(alpha=0.2, fill="red")+
    geom_histogram(aes(`X2.1`/`X1`), alpha=0.2, fill="blue")+
    geom_histogram(aes(TC2), alpha=0.2, fill="darkblue")+
    geom_histogram(aes((`X2.1`)/200), fill="darkred",alpha=0.2)+
    geom_text(x=0.2, y=1000, label="true proportion species", color="darkred")+
    geom_text(x=0.8, y=1000, label="obs/Chao1 proportion", color="red")+
    geom_text(x=0.5, y=700, label="estimated coverage", color="blue")+
    geom_text(x=0.6, y=800, label="true coverage", color="darkblue")+
    theme_classic()+labs(x="")+
    xlim(c(0,1))
dev.off()

mydf
mydf %>% group_by(SS, order) %>% summarize(expected=mean(qD), vard=sd(qD), iftvard= (sum(qD.UCL-qD.LCL)/(5000*1.96))
                                           , toolo=sum(qD.LCL>=expected)/n()
                                           , toohi=sum(qD.UCL<=expected)/n(), isright=1-sum(toohi, toolo)
                                           , varn=sd(m), meann=(mean(m)))
