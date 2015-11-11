### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: examples.Rout

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk

include stuff.mk

##################################################################

Sources += $(wildcard *.R)

functions.Rout: functions.R
	$(run-R)

examples.Rout: functions.Rout examples.R

##################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
Makefile: start.makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/wrapR.mk

# -include $(ms)/oldlatex.mk
