## This is diversity_metrics 
## makestuff/project.Makefile

current: target
-include target.mk

# include makestuff/perl.def
Drop = ~/Dropbox
Ignore += bibdir

##################################################################

## Early examples
## go examples.Rout.pdf

subdirs += scripts

Sources += $(wildcard *.R)

functions.Rout: functions.R
	$(run-R)

examples.Rout: functions.Rout examples.R

checkplots.Rout: scripts/helper_funs/estimation_funs.R checkplots.R
	$(run-R)

##################################################################

md = $(wildcard *.md)
mh = $(md:md=html)

Sources += $(md)
Ignore += $(mh)

## Derivations, simplifications, dead ends
notes.html: notes.md

## Stuff we sort of want to say
outline.html: outline.md

## Draft MS

Sources += rarity.tex
## rarity.pdf: rarity.tex

## Dump from MR via email
Sources += prospectus.md diversity.md

######################################################################

## Scaling illustrations (to be merged?)

Sources += $(wildcard *.R)

Ignore += rarity_seesaws_1.pdf
rarity_seesaws_1.pdf: scaled_means.Rout ;

scaled_means.Rout: scaled_means.R 

balance.Rout: balance.R

adaptive.Rout: adaptive.R

Ignore += scaled_means.debug
scaled_means.debug: scaled_means.Rout
	perl -ne "print " $< > $@

scaled_means_other.Rout: scaled_means_other.R
scaled_means_other.R: scaled_means.R.36cf85c9.oldfile
	$(copy)

Gods_Unbiased_Estimator.Rout: Gods_Unbiased_Estimator.R

######################################################################

## Reference stuff

Sources += auto.rmu
auto.html: auto.rmu

Ignore += library
library: dir=$(Drop)/mean_rarity_lit
library:
	$(linkdirname)

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls $@

-include makestuff/os.mk
-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
-include $(ms)/texdeps.mk
-include $(ms)/autorefs.mk
-include $(ms)/pandoc.mk
-include $(ms)/wrapR.mk
