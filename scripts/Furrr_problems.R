# load furrr, describe "plan"
library(purrr)
library(furrr)
nc<-2
plan(strategy = multisession, workers = nc)

# create objects

a<-list("A", "B", "C")
b<-list("D", "E", "F")

just_a_counter<-1
#works as expected
map_dfr(c("a", "b"), function(my_object_name){
    bar<-get(my_object_name)[[3]]
    #give the function something to do so it uses plan
    new_data<-sample(1:1e5, 1e3, replace=T) 
    print(bar)
    data.frame(t(new_data), bar, just_a_counter)
})

#works fine
newdf<-future_map_dfr(1:5, function(foo){
    map_dfr(1:100, function(just_a_counter){
        map_dfr(c("a", "b"), function(my_object_name){
            bar<-my_object_name
            #give the function something to do so it uses plan
            new_data<-sample(1:1e2, 1e2, replace=T) 
            print(bar)
            data.frame(t(new_data), bar, just_a_counter)
        })
    })
})


# object 'a' nnot found

newdf<-future_map_dfr(1:5, function(foo){
    map_dfr(1:10, function(just_a_counter){
        map_dfr(c("a", "b"), function(my_object_name){
            bar<-get(my_object_name)[[3]]
            #give the function something to do so it uses plan
            new_data<-sample(1:1e5, 1e2, replace=T) 
            print(bar)
            data.frame(t(new_data), bar, just_a_counter)
        })
    })
})
