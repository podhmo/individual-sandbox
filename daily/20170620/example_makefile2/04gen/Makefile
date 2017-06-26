NUMS = $(shell jot 3 1 | sed 's/^/0/g')

default: $(foreach i,${NUMS},do$i)

define _genTask
do$1:
	@echo start $1
	sleep 1
	@echo end $1
endef

$(foreach i,${NUMS},$(eval $(call _genTask,$i)))
