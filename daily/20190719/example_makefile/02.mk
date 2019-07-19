find = $(wildcard $(1)) $(wildcard */$(1))
pypackages = $(dir $(call find,setup.py))

02:
	$(info @ $(pypackages) @)
	$(info @ $(sort $(call find,*.mk)))
