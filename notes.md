Poisson logic
=============

If the community size is large, then we expect the representation of a given rare species to follow a Poisson distribution, with mean μ=n p_r. This gives the following probabilities that it will be:

* absent: exp(-μ)
* singleton: μ exp(-μ)
* doubleleton: μ² exp(-μ)/2

Thus, the expected _number_ of species in these categories are:

* absent: f₀ = S_r exp(-μ)
* singleton: f₁ = S_r μ exp(-μ)
* doubleleton: f₂ = S_r μ² exp(-μ)/2

Chao1 estimates 

Dead-simple Chao1
=================

This is an attempt to give the simplest explanation of Chao1 estimators that we can come up with.

Chao1 estimation is effectively based on what our group calls the Good-Turing 1 (GT1) assumption: all singletons and doubletons observed are from a group of rare species with the same relative abundance. This seems like an extreme assumption. There is much to be said about why it's roughly OK, and what other formal assumptions can get you to the same effective place, and maybe we'll come back to that.

Notation
--------

We have S_r rare species, each with a relative abundance of p_r. The sample size is n. 

Derivation
----------

If the community size is large, then we expect the representation of a given rare species to follow a Poisson distribution, with mean μ=n p_r. This gives the following probabilities that it will be:

* absent: exp(-μ)
* singleton: μ exp(-μ)
* doubleleton: μ² exp(-μ)/2

Thus, the expected _number_ of species in these categories are:

* absent: f₀ = S_r exp(-μ)
* singleton: f₁ = S_r μ exp(-μ)
* doubleleton: f₂ = S_r μ² exp(-μ)/2

What we want is to add an estimate of the unobserved number absent. We can construct such an estimate by assuming that f₁ and f₂ take their expected values above. Then the expected number absent is just given by f₁²/(2 f₂). 

In other words, S-hat = S_obs +  f₁²/(2 f₂), where S-hat is the richness estimate, and S_obs is the number of species observed. 

The Chao1 estimator is really just that simple (although not all of its justifications are).

Rarity perspective
==================

Does the GT1 assumption map to a mean-rarity point of view? If so, does it give the Chao1 estimator or something else?

Baseline
--------

The mean rarity estimate of S is the geometric mean of rarity.

We can write this across species s:

Sum_s{p_s r-hat(f_s)}, where f_s is the number of samples of a gi

Or, since r-hat is just a function of f, we can sum by categories of species with f observations:

Sum_f{f/n) r-hat(
