source("scripts/checkplot_initials.R")
source("scripts/checkplot_inf.R")
reps<-50
outerreps<-1000
nc<-50
plan(strategy=multisession, workers=nc)
map(rev(1000+1:1000+outerreps), function(x){
    map(rev(round(10^seq(2, 5, 0.25))), function(size){
        map(c(-1, 0, 1), function(l){
            #map(1:length(flatten(flatten(SADs_list))), function(SAD){
            map(c(1,8,22), function(SAD){
            
        start<-Sys.time()
out<-checkplot_inf(flatten(flatten(SADs_list))[[SAD]], l=l, inds=size, reps=reps)
write.csv(out, paste("data/SAD_special",SAD, "l",l,"inds", size, "outernew",  x, ".csv", sep="_"), row.names=F)
rm(out)
print(Sys.time()-start)
            })
        })
    })
})
