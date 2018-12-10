[TODO]


# Diversity is mean rarity

This matters for interpretation and estimation.

## Interpretation

Mean rarity implies something fascinating, but not directly useful: there exists an unbiased (and usually good) diversity estimate (on any scale, including richness) that considers only species in a sample, without accounting for unsampled species, even implicitly! Obviously, this is a trick, and the trick is that we would have to know the exact rarity (or, equivalently, relative abundance) of each of these species, which is generally supposed to be impossible. Nonetheless, thinking about this "God estimate" can provide useful guidance.

Shannon corresponds to logarithmic scaling and the geometric mean (see pictures). This will often, but not always, be the natural scale. Roughly in the same way that its usually better to say 10:20 as 1000:2000 (rather 1000:1010 or 1000:transinfinity)

## Scales

Our indices are most naturally _considered_ on what we will call the "numbers scale", and _computed_ on what we will call the "index scale". The numbers scale corresponds to the effective number of species, _sensu_ Jost. Diversity estimates on the numbers scale [TODO something]. Importantly, however, they are not particularly useful for comparing across index types (e.g., between Simpson, Shannon and richness). We will call values on the numbers scale numbers, and values on the index scale indices, so that we can talk about a Simpson number as a function of the Simpson index, for example. The richness number and richness index are the same, so we will refer simply to richness.

## Estimation

### Bias

Unbiased estimates always sound like a good idea, and sometimes they are. But estimates that are unbiased on one scale will typically be biased on other scales. 

To see this we recall that the definition of unbiased is that the _mean_ of hypothetical large ensemble of estimates would approach the true vale. Thus, applying a non-linear transformation before taking the mean will generally convert an unbiased estimate to a biased estimate. In particular, the unbiased estimates available for the Simpson index yield biased estimates for Simpson numbers. Specifically, since [TODO something tricky about curvature], the estimates are biased high on the numbers scale. This is not necessarily a problem. As sample size increases, the estimate converges to the right answer, which is the same on either scale, and it is therefore _asymptotically_ unbiased on both scales.

So biased estimates of _mean_ rarity are not necessarily a problem. Biased estimates of individual or species rarity are more problematic: the index estimate is based on an average of these rarities, and if they are biased, there is no reason why the mean rarity estimate should converge. Specifically, while estimators unbiased on the Simpson scale produce consistent estimates of Simpson indices and numbers, they produce inconsistent, high estimates of Shannon or richness diversities.

### Ways to estimate rarity

We are super-interested in ways to estimate species rarity and whether (and how) they're biased on particular scales.

An estimator that is unbiased on the arithmetic scale is one we call "God's estimator." It is not practical for observed data, but is informative to compare other estimators against. 

God's estimator works by plugging in the true rarity for each observed species. 

Formally, if we use our notation from the notes.md file, we have God's esitmator as 

s~hat~God= sum[ i=1 -> S~total~] \( n~i~ * r~i~ \) /sum[ i=1 -> S~total~] \( n~i~ \)



Where S is the total # species in the community
n_i is the number of individuals from species i observed in the community
r_i is the true, unobserved rarity of species i in the community. 

It follows that God, while not always correct, is not stupid. That is to say, God never predicts a mean rarity< S_obs, the number of species God already observed in a given sample, using this estimator.

The minimum addition an arbitrary species can have to this sum is 1; No r_i<1 can exist. This implies that God's estimator is always greater than a naive estimator, which predicts S_obs, subsituting 

(sum[i=1->S_total](n_i))/n_i for God's r_i.

#more estimators

Here's a list of some of the ones that do or should exist

* Probability based unbiased Simpson estimators

* Simpson-Hill estimator to be tracked by MR.

* Poisson based. Worked out during Hamilton visit. These apply Good-Turing logic to singletons and doubletons, recover coverage and Chao estimates, and produce disappointing estimates ☹ (smaller than Chao1

* Formal Chao1: Is there a logic that ties Chao1 to a set of specific rarity estimates? If so, does it provide any insights?

* Almost unbiased Shannon estimates.

* Good based. These would use Good's formula, with or without regularization.

* Fancy. These would use expansions (like Efron, Haegeman, newer Chao)
