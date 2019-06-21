Fake community for bootstrapping
--------------------------------

Chao's Bt_prob_abu function is a fairly deep attempt to make a fake community for bootstrapping, but it doesn't seem to work so well for richness (in fact, maybe doesn't work super-well at all, since there seems to be a mysterious mean-correction factor operating).

We could try to estimate coverage _indirectly_, by finding a fake community whose expected (richness; Simpson) matches the observed. I hesitate to jump into that only because it seems possible to match Shannon as well, but how to do that raises estimation questions that I could get lost in for a long time.

To do just richness and Simpson should be pretty straightforward.

Let n = Σn_i be the total number of individuals sampled.

Assume α_i = C n_i/n. Call the new species abundances β_j. The mean is (1-C)/u, where u is the number of unknown species.

If we set the population Simpson index equal to the sample-estimate Simpson index of the sample, we can solve for a proposed value of u. Thus, we should be able to use uniroot to find a value of C so that expected richness is equal to observed richness.

Finally, we should go back to the sample and fiddle u and β. We should make û a whole number: ceiling(u) or round(2*u) and then find β values to match Simpson exactly. We could also match Shannon exactly if we knew how.

Need to stop messing with this. If and when I come back: spin these out into separate functions in a separate file, and check what they do step by step. 2019 Jun 21 (Fri)
