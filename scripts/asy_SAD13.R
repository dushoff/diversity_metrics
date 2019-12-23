source("scripts/checkplot_initials.R")
source("scripts/checkplot_inf.R")
reps<-125
outerreps<-400
nc<-125
plan(strategy=multiprocess, workers=nc)
map(c(-1,0,1), function(l){
map(1:outerreps, function(x){
    map(rev(round(10^seq(2, 5, 0.25))), function(size){
        start<-Sys.time()
out<-checkplot_inf(flatten(flatten(SADs_list))[[13]], l=l, inds=size, reps=reps)
write.csv(out, paste("data/SAD12","l", l, "inds", size, "outer",  x, ".csv", sep="_"), row.names=F)
print(Sys.time()-start)
})
  })
  })
})
