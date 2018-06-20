### Hooks for the editor to set the default target
current: target
-include target.mk

##################################################################

# make files

Sources = Makefile .ignore README.md

Ignore += .gitignore

Sources += Makefile .ignore 
Ignore += .gitignore

msrepo = https://github.com/dushoff
ms = makestuff

Drop = ~/Dropbox
Ignore += local.mk
-include local.mk
-include $(ms)/os.mk

Sources += $(ms)
Makefile: $(ms) $(ms)/Makefile
$(ms):
	git submodule add -b master $(msrepo)/$(ms)

## Only meant to work with makestuff.sub
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

Sources += notes.md

## Draft MS

Sources += rarity.tex
rarity.pdf: rarity.tex

######################################################################

## Reference stuff

Sources += auto.rmu
auto.html: auto.rmu

Ignore += library
library: dir=$(Drop)/rarity_docs
library:
	$(linkdirname)

######################################################################

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/texdeps.mk
-include $(ms)/autorefs.mk
-include $(ms)/pandoc.mk
-include $(ms)/wrapR.mk
