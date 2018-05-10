### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: examples.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md sub.mk

## I wrote somewhere not to do this, but I think the best thing is to 
## not track .gitignore
Ignore += .ignore

include sub.mk

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

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/texdeps.mk
-include $(ms)/wrapR.mk

# -include $(ms)/texdeps.mk
