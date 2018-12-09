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

So biased estimates of _mean_ rarity are not necessarily a problem.

### Ways to estimate rarity
