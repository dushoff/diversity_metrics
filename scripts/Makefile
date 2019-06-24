
current: target
-include target.mk

##################################################################

## Defs

Sources += Makefile

ms = makestuff
Ignore += $(ms)
Makefile: $(ms)/Makefile
	touch $@
$(ms)/Makefile:
	ls ../makestuff/Makefile && /bin/ln -s ../makestuff 

-include $(ms)/os.mk

######################################################################

Sources += $(wildcard *.R helper_funs/*.R)
richness_stats.Rout: usersguide.Rout helper_funs/estimation_funs.Rout helper_funs/prettify.Rout richness_stats.R
	$(run-R)

newboot.Rout: usersguide.Rout helper_funs/fiddle.Rout helper_funs/estimation_funs.Rout newboot.R
	$(run-R)

diversity_checkplots_richness_only.Rout: helper_funs/estimation_funs.Rout helper_funs/prettify.Rout diversity_checkplots_richness_only.R

Sources += $(wildcard *.md)

######################################################################

### Makestuff

-include $(ms)/wrapR.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk

