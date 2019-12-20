source("scripts/checkplot_initials.R")
source("scripts/obscp_inf.R")
reps<-5e3
Bnum<-2e3
nc<-10 #scaling this back to work on amarel... suspect memory issues
plan(strategy=multiprocess, workers=nc)
 map(round(10^seq(2, 5.5, 0.25)), function(size){
map(c(-1,0,1), function(ell){
map(1:10, function(tryme){
        start<-Sys.time()
nd<-trycheckingobs(flatten(flatten(SADs_list))[[23]], size, ell)
write.csv(nd, file=paste("data/new_trycheckingobs_SAD_23", "iter_", tryme, "size", size, ".csv", sep=""), row.names=F)
print(Sys.time()-start)
})
})
})
