

Diversity is mean community rarity



The goal of this document is to re-derive Hill Numbers (Chao & Jost 2015, Chao et al. 2014, Haegeman et al. 2013, Hill 1973, Jost 2010, MacArthur 1965)[MR1]  using rarity, rather than commonness. Rarity and commonness are reciprocals of eachother, where the commonness, i.e. relative abundance, of species i  in community j with S species is equation 1:  , and rarity is simply equation 2: . In most ecological communities, neither cij  nor rij is known a priori, and these quantities must be estimated from sample data. A reasonable approximation for cij is the observed relative abundance of species i, pij. It is well known that pij is a positively-biased estimator of c­Ij, and that the precision decreases and bias increases for small pi (Chao & Jost 2015). For the sake of our argument, however, we’ll consider that  c­ij=1/ rij ~pij.[MR2] 

            It[MR3]  may be helpful to consider a jar of colored marbles. Let’s imagine that for each color of marble, the glass has a characteristic fraction of magnesium in it, and we we can measure this fraction with a fancy spectrometer. Then, we could ask how much magnesium there is in typical marbles, based on the colors of the marbles in a jar. If the jar has only a few marbles, we could census them, and ascertain precise values of magnesium for each color, for that matter, for each marble, in the jar. More likely, we’ll have to make simplifying assumptions that the jar is well-mixed and our probability of selecting an individual marble is not associated with its color, and infer typical magnesium concentrations based on incomplete samples. [MR4] 

            To determine the average fraction of magnesium in the marbles in the jar, each time we grabbed a new color, we would measure its magnesium content. Then, we could count the number of observations of each color. We would use these counts as fractions of the total to weight the magnesium proportions associated with each glass color in our average, using the generalized weighted mean of n quantities xi

, where wi is the weight associated with quantity xi. The mean of a set of n quantities always lies between the minimum and maximum quantity (inclusive), but unless all the quantities are identical, the center of a distribution of n numbers is somewhat subjective. The exponent l defines where the center lies; larger l values stress the larger xi’s and place center upwards, while smaller l values stress the smaller xi’s, placing the center lower. In our magnesium example, we would take the observed fraction of all the marbles that are color i (wi), and multiply this by the fraction of each marble of color i that is magnesium (xi ), raised to the l power, take the sum of this product across all n colors, and take the l’th root.

The weights in our sample are exactly observed… we count the number of marbles of each color. These proportions may not accurately represent the proportions of marbles of each color n the whole jar; rather, we weight our average by the frequency with which we observe the colors. The magnesium content is more of an estimated quantity, and we may not know the variance associated with it across different colors. We may know that marbles of the same color are cast on the same machine, and that there is a true magnesium content in the glass used by each machine. The content we observe in individual marbles of the same color represents random variation around this color-level magnesium content. There is a great deal of statistical theory available for us to attend to this matter.

It is obvious that magnesium content is both a property of an individual marble, and also of all marbles manufactured from the same pool of molten glass (i.e. marbles of the same color). As ecologists, we could imagine that our marbles are individuals, where colors represent species. Rather than measuring magnesium content, we could measure traits… like body mass. This invites an awkward question: is rarity a trait?

We’ll presuppose that indeed, rarity is a trait, a proposition to examine later. First, let’s trace this intuition for a moment. We take rarity to be a species trait: a property of an entire species that is also a property of each individual that belongs to it. We’ll imagine that coincidentally, this trait is inversely related to the frequency with which we detect species. Using the observed frequency of species i, pi to weight the rarity of species i, ri (estimated as 1/pi,) we could take the average community rarity as . You may note that is the well-known Hill diversity slightly in disguise… rewriting l as 1-q we have . Not only is this measure of mean rarity also the mathematical definition of diversity (Chao et al. 2014, Ellison 2010, Haegeman et al. 2013, Hill 1973, Jost 2006, Tuomisto 2010), but it is also a statistical model for estimating diversity given incomplete sampling.

For each species i in community j, rij  is not known, only estimated. In our measure of average community rarity, we weight this estimate by the number of times we observe individuals with that species-level rarity, the known sample frequency pij. The rarer typical individuals are in a community, the more diverse we say it to be. However, the rarer individual are in a community, the less certainy we have about their rarity, as well. The power of the exponent in the generalized mean is the ability to stress the larger numbers (the rarer species) even though their weights are small, or to stress the smaller numbers (more common ones), allowing their weights to dominate.  

Although this is a neat idea, it leaves some big questions. What does this intuition about rarity mean for some crucial components of estimating diversity? How do we deal with the fact that the rarity of individuals is in fact not the least bit independent? That it isn’t really a property of individuals at all, and that it cannot be observed from individuals? That with increased sampling, we expect the rarity of already sampled individuals to change (and typically to increase)? Also, the mean rarity of the community increases with sampling. How about predicting the rarity of as-yet unseen species? How does conceptually divorcing an observed frequency (pij) from the unobserved, true rarity rij help us understand the relationship between p and r?

________________________________

 [MR1]JD’s first comment: start with a streamlined summary of, for example, Jost 2010, explaining what Hill numbers are and why they’re cool and that Ecologists should use them, before jumping into this stuff.

 [MR2]JD has some thoughts on a more precise way to think about this that he shared but I haven’t read yet.

 [MR3]Before jumping into thinking about the issue of  sampling and the differences between weights from observed counts, and the thing to estimate i.e. rarity, just say that if we knew all rarity, then Hill numbers ARE mean rarity, exactly, and obviously

 [MR4]JD sounds excited by this way of thinking about rarity as a way to tease out the sampling questions. He suggests that for next draft, though, focus on the comments above.



