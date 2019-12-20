source("scripts/checkplot_initials.R")
source("scripts/obscp_inf.R")
reps<-5e3
Bnum<-2e3
nc<-50 #scaling this back to work on amarel... suspect memory issues
plan(strategy=multiprocess, workers=nc)
 map(round(10^seq(2, 5.5, 0.25)), function(size){
map(c(-1,0,1), function(ell){
map(1:100, function(tryme){
nd<-trycheckingobs(flatten(flatten(SADs_list))[[17]], size, ell, rf=100)
write.csv(nd, file=paste("data/new_trycheckingobs_SAD_17", "iter_", tryme, "size", size, ".csv", sep=""), row.names=F)
})
})
})
