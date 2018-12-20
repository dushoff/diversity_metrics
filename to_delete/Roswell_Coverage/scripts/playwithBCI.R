require(iNEXT)


BCI<-read.csv("Roswell_Coverage/data/BCI.csv")
head(BCI)
str(BCI)

gs<-paste(BCI$Genus, BCI$Species, sep="_" )
bci<-as.data.frame(cbind(BCI$Sampling.Unit.Name, BCI$Subplot.Number, gs))
str(bci)
names(bci)<-c("site", "subplot", "gs")
str(bci)
levels(bci$gs)
bci<-bci[which(bci$gs!="Unknown_unknown"),]
str(bci)

?table
counts<-table(bci$gs)
str(counts)
str(counts[1])
counts<-as.matrix(counts)
iNEXT(counts[,1])
counts[,1]


