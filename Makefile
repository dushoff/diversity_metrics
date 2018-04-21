### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: examples.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md sub.mk

include sub.mk

##################################################################

Sources += $(wildcard *.R)

functions.Rout: functions.R
	$(run-R)

examples.Rout: functions.Rout examples.R

##################################################################

### Makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/wrapR.mk

# -include $(ms)/texdeps.mk
