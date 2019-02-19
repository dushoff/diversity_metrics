# show what scales look like 
library(tidyverse)
library(ggthemes)

#read in a dataset with some values and then some mappings for the transformations
pts<-read.csv("data/scales_plot.csv")

#just did line segments manually
x<-c(2,1,0,-1)
xend<-x
y<-c(75,100,56.9837097,66.666466)
yend<-c(-60, -100, -56.9837097, -66.6666666)
segs<-data.frame(x=x, xend=xend, y=y, yend=yend)

#titles for each scale
tits<-data.frame(x=c(2,1,0,-1), y=rep(135,4), lbl=c("quadratic", "arithmetic\n(linear)", "logarithmic", "reciprocal"))

#plot
cairo_pdf(height=3.5, width=9, filename="figures/just_scales_h.pdf")
# quartz(height=3.5, width=9)
(pts %>% 
        ggplot(aes(yfixed, ell))
    +geom_segment(aes(y=ell-0.1, yend=ell+0.1, x=yfixed, xend=yfixed
                      , color=colr), size=0.8)
    +geom_segment(data=segs, aes(y=x, yend=xend, x=y, xend=yend), size=0.8)
    +scale_color_colorblind()
    +geom_text(aes(y=ell-0.2,label=paste(lab), color=colr)
               , size=3.2) #, fontface="bold")
    +geom_text(data=tits, aes(x=y, y=x, label=lbl), size=4.7)
    +scale_y_continuous(limits=c(-1.3,2.1))
    +scale_x_continuous(limits=c(-100,160))
    +labs(y="scaling exponent\n")
    +theme_classic()
    +theme(axis.ticks.x = element_blank(), axis.text.x=element_blank()
           , axis.title.x = element_blank(), axis.line.x = element_blank()
           , axis.text.y=element_text(size=14), axis.title.y=element_text(size=14)
           # , axis.line.y=element_blank(), # axis.ticks.y=element_blank()
           , legend.position = "none")
)
dev.off()

