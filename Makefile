### Hooks for the editor to set the default target
current: target
-include target.mk

##################################################################

# make files

Sources = Makefile README.md

msrepo = https://github.com/dushoff
ms = makestuff

Drop = ~/Dropbox
-include $(ms)/os.mk

Sources += $(ms)
$(ms)/%.mk: $(ms) $(ms)/Makefile ;
$(ms)/Makefile:
	git submodule update -i

##################################################################

## Early examples
## go examples.Rout.pdf

Sources += $(wildcard *.R)

functions.Rout: functions.R
	$(run-R)

examples.Rout: functions.Rout examples.R

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

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/texdeps.mk
-include $(ms)/autorefs.mk
-include $(ms)/pandoc.mk
-include $(ms)/wrapR.mk
