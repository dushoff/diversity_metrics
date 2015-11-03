### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: notarget

##################################################################


# make files

Sources = Makefile .gitignore README.md

######################################################################

msrepo = https://github.com/dushoff
gitroot = ../

-include local.mk
-include $(gitroot)/local/local.mk
ms = $(gitroot)/makestuff

##################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
Makefile: start.makestuff

%.makestuff:
	-cd $(dir $(ms)) && mv -f $(notdir $(ms)) .$(notdir $(ms))
	cd $(dir $(ms)) && git clone $(msrepo)/$(notdir $(ms)).git
	-cd $(dir $(ms)) && rm -rf .$(notdir $(ms))
	touch $@

-include $(ms)/git.mk
-include $(ms)/visual.mk

# -include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
