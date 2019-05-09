# use selected command
00: SED ?= $(shell which gsed 2>/dev/null || which sed)
00:
	echo ${SED} -i 's/a/@/g' target.txt


# branching by conditions
01:
ifndef XXX
	$(error XXX is not found)
endif
	echo hai ${XXX}

## else
02:
ifndef XXX
	echo not found
else
	echo hai ${XXX}
endif
	$(info hai)
