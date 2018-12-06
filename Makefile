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

Ignore += notes.html
Sources += notes.md
notes.html: notes.md

## Draft MS

Sources += rarity.tex
## rarity.pdf: rarity.tex

## Dump from MR via email
Sources += prospectus.md diversity.md

######################################################################

## Scaling illustrations (to be merged?)

Sources += $(wildcard *.R)

scaled_means.Rout: scaled_means.R 
scaling_not_weighting.Rout: scaling_not_weighting.R

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
