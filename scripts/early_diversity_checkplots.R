###############
# file to read in checkplot data and make some CHECKPLOTS!

mycps_sofar<-map_dfr(1:10, function(x){
      map_dfr(1:24, function(SAD){
        map_dfr(rev(round(10^seq(2, 4, 0.25))), function(size){
          map_dfr(c(-1,1), function(l){
                
                out<-try(read.csv( paste("data/SAD", SAD, "_l_", l, "_inds_", size, "_outer_",  x, "_.csv", sep="")))
                print(SAD, size, l)
                return(data.frame(out, SAD=SAD))
          })
        })
      })
})    

pdf("figures/first_new_cps.pdf", height=4.5, width=8)
mycps_sofar %>% filter(!(inds %in% c(3162, 5623, 316,178, 1778, 31623, 56234, 17783, 10000))) %>% 
    mutate(hilld=c("richness", "Hill-Shannon", "Hill-Simpson")[2-l]) %>% 
    mutate(hilld=factor(hilld, levels=c("richness", "Hill-Shannon", "Hill-Simpson"))) %>% 
    filter(hilld!="Hill-Shannon") %>% 
    checkplot(facets=8)+
    theme_classic()+
    facet_grid(hilld~inds, scales="free")+
    theme(panel.spacing.x = unit(2, "lines"))+
    scale_x_continuous(expand=c(0,0))
  
dev.off()
m<-1
pdf(file="figures/diversity_slugs_early.pdf", height=3.5, width=3.5)
map(c(-1, 1), function(m){
        k<-mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>% 
            filter(inds==100& l==m) %>% 
            mutate(est=chaoest) 

        
        
        rangePlot(k, target=mean(k$truediv)
                  , title=c("richness", "Hill-Shannon", "Hill-Simpson")[2-m], opacity=0.05)+
            theme_classic()
        
         
})

dev.off()

pdf(file="figures/diversity_slugs_early_Simp_10000.pdf", height=3.5, width=3.5)
rangePlot(mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>% 
    filter(inds==10000& l==-1) %>% 
    mutate(est=chaoest) ,
    target=10
    , title=c("richness", "Hill-Shannon", "Hill-Simpson")[3], opacity=0.05)+
    theme_classic()
dev.off()

pdf(file="figures/diversity_slugs_early_RICH_10000.pdf", height=3.5, width=3.5)
rangePlot(mycps_sofar[seq(25, length(mycps_sofar$p), 25),] %>% 
              filter(inds==10000& l==1) %>% 
              mutate(est=chaoest) ,
          target=200
          , title=c("richness", "Hill-Shannon", "Hill-Simpson")[1], opacity=0.05)+
    theme_classic()
dev.off()
