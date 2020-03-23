# pick points for high density regions

#xobs<-runif(900)

clustery_obs<-function(x){rnorm(40,x,)}
under_x<-runif(11)
xobs<-sapply(under_x, clustery_obs)
xobs<-xobs[0<xobs&xobs<1]

yfun<-function(x){rnorm(1, mean=(10*x)^2-(30*x)+3, sd=14*x)}

yobs<-sapply(xobs, yfun)

plot(xobs, yobs)

