source("scripts/checkplot_initials.R")
source("scripts/checkplot_inf.R")
reps<-50
outerreps<-1000
nc<-7
plan(strategy=multisession, workers=nc)
map(rev(1:outerreps), function(x){
    map(rev(round(10^seq(2, 5, 0.25))), function(size){
        map(c(-1, 0, 1), function(l){
            map(c(7, 15), function(SAD){
            
        start<-Sys.time()
out<-checkplot_inf(flatten(flatten(SADs_list))[[SAD]], l=l, inds=size, reps=reps)
write.csv(out, paste("data/SAD_special",SAD, "l",l,"inds", size, "outernew",  x, ".csv", sep="_"), row.names=F)
rm(out)
print(Sys.time()-start)
            })
        })
    })
})
