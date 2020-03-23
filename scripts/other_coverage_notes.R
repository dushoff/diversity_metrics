# plot the coverage coverage test

# 7) Find a fun way to plot the upshot: whether statistical coverage of 95%, whether misses are 2.5% high and 2.5% low, how this depends on sample size, coverage, and Hill number.
# 
# 8) Bonus, but might want to do this earlier and with a simple small dataset: biplot of true coverage and Chao's estimated coverage. Lost track of data from the other day and so going to backburner this for now and get all of this up and running on big computer asap.
# 

# baseline_samples %>% 
#     gather(dtype, diversity, rich, simp, shan) %>% 
#     ggplot(aes(tc, diversity, color=comm))+
#     geom_point()+
#     facet_wrap(~dtype)+
#     theme_classic()