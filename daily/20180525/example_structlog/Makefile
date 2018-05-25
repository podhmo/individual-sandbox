define runT
	python $(1) 2>/dev/stdout | tee $(1).log

endef

default:
	$(foreach f,$(wildcard *.py),$(call runT,$(f)))
