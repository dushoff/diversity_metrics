source("scripts/checkplot_initials.R")
source("scripts/obscp_inf.R")
reps<-5e3
outerreps<-400
Bnum<-2e3
nc<-125#per Rob's recommendation
plan(strategy=multiprocess, workers=nc)
 map_dfr(round(10^seq(2, 5.5, 0.25)), function(size){
map(1:10, function(tryme){
        start<-Sys.time()
nd<-trycheckingobs(flatten(flatten(SADs_list))[[7]], size)
write.csv(nd, file=paste("data/new_trycheckingobs_SAD_7", "iter_", tryme, "size", size, ".csv", sep=""), row.names=F)
print(Sys.time()-start)
})
})
