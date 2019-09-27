require(tidyverse)

#make radplot
radplot<-function(comm, maxrich=length(comm), maxab=max(comm), fill, shape=16){
    comm<-comm[comm!=0]
    rawrnk<-tibble(abund=comm, rnk=row_number(comm))
    toplot<-rawrnk %>% 
        mutate(x=-rnk-maxrich+max(rnk))
    
    f<-(toplot %>% ggplot(aes(x, abund, size))
        +geom_point(shape=shape, color=fill, size=1.8)
        +geom_line(color=fill)
        + scale_x_continuous(limits=c(-maxrich, 0))
        + scale_y_continuous(limits=c(0,maxab+2), expand=c(0,0))
        +theme_classic()
        +theme(axis.text.x = element_text(color="white"), axis.text.y=element_text(colour="black")
               , legend.position="none", text=element_text(size=16))
        +labs(x="abundance rank", y="individuals")
    ) 
    return(f)
    
}