source("/home/mr984/diversity_metrics/scripts/checkplot_initials.R")
source("/home/mr984/diversity_metrics/scripts/checkplot_inf.R")
reps<-50
outerreps<-1000
size<-rev(round(10^seq(2, 5, 0.25)))[
12
]
nc<-12
plan(strategy=multisession, workers=nc)
map(rev(1:outerreps), function(x){
        start<-Sys.time()
out<-checkplot_inf(flatten(flatten(SADs_list))[[6]], l=0, inds=size, reps=reps)
write.csv(out, paste("/scratch/mr984/SAD6","l",0,"inds", size, "outernew",  x, ".csv", sep="_"), row.names=F)
rm(out)
print(Sys.time()-start)
})
