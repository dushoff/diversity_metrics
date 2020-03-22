# testing statistical coverage of coverage still to do
# 4)  for each SAD, bin observations, say 100 at a time, by true coverage. This means that in each bin you'd have 100 samples of very similar true coverage, they may have a variety of different sample sizes.
# 
# 5) For each bin, make a boxplot or similar for each Hill number. Goal is to visualize mean diversity given coverage, and the distribution. Should be clean-looking. Record mean diversity, mean coverage for each bin, hill number.
# 
# do similar to before but this time focus on chao estimated diversities

#     a) if target sample coverage is above or below true sample coverage for that sample
#     b) for each Hill diversity, if expected diversity for that coverage is in, below, or above Chao's interval
# c) sample stats like actual sample size, diversity, coverage
# 
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