source("scripts/checkplot_initials.R")
source("scripts/obscp_inf.R")
reps<-5e2
Bnum<-2e3
nc<-36 #scaling this back to work on amarel... suspect memory issues
plan(strategy=multisession, workers=nc)
 map(round(10^seq(2, 3.5, 0.25)), function(size){
map(c(-1,0,1), function(ell){
map(1:100, function(tryme){
        start<-Sys.time()
nd<-trycheckingobs(flatten(flatten(SADs_list))[[12]], size, ell)
write.csv(nd, file=paste("data/new_trycheckingobs_SAD_12", "iter_", tryme, "size", size, ".csv", sep=""), row.names=F)
rm(nd)
print(Sys.time()-start)
})
})
})
