
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

Sources += $(wildcard *.R)
richness_stats.Rout: helper_funs/estimation_funs.Rout helper_funs/prettify.Rout richness_stats.R
	$(run-R)

######################################################################

### Makestuff

-include $(ms)/wrapR.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk

