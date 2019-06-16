
* [Dushoff's notebook notes](http://dushoff.github.io/notebook/diversity.html)

### R scripts in scripts/

*Gods_Unbiased_Estimator.R* compares mean rarity estimates where true rarities are known and sample abundances used in mean estimation (Gods unbiased estimator) to diversity estimates from other (less divine) realms

*just_scales.R* unsophisticated script to plot the different scales, with no data besides a handful of reference points


*balance.R* includes functions to take an observed SAD and plot abundance and rarity on various scales with mean as the fulcrum

*simpson.R* shows ability to estimate Hill-Simpson with sampling

*binom_checkplots.R* rudimentary exploration of binomial test p-value approximations

*diversity_checkplots.R* generates check plots and plot of true value coverage for sample diversity and asymptotic estimators using Chao and Jost 2015 proposed CI

*assess_coverage.R* compares relative diversity for 3 hill numbers when using coverage, size, or asymptotic estimators with random samples of different sizes

*helper_funs/* A directory containing helpful functions for rarefaction, computing diversity, etc. used by one or more scripts in the main scripts directory



## md files
*notes.md* currently has some to-do notes and various bits on Chao estimation and generalizing the good-turing logic to estimating rarity

*outline.md* is a rough MS outline

*prospectus.md* is MS prospective