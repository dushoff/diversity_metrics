source("scripts/checkplot_initials.R")
source("scripts/checkplot_inf.R")
reps<-50
outerreps<-1000
nc<-12
plan(strategy=multicore, workers=nc)
map(1:outerreps, function(x){
    map(rev(round(10^seq(2, 5, 0.25))), function(size){
        start<-Sys.time()
out<-checkplot_inf(flatten(flatten(SADs_list))[[13]], l=-1, inds=size, reps=reps)
write.csv(out, paste("data/SAD13","l",-1,"inds", size, "outer",  x, ".csv", sep="_"), row.names=F)
rm(out)
print(Sys.time()-start)
})
  })
})
