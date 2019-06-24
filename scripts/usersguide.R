library(mobsim)

set.seed(1444)

s_pool=60 # Pool 
n_ind=1e3 #number of individuals to simulate
sad_coef=list(cv_abund=5)

usersguide<-as.numeric(sim_sad(s_pool=s_pool 
	, n_sim=n_ind #number of individuals to simulate
	, sad_coef=sad_coef
))


