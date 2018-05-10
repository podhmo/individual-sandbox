define testT =
	(cd $(dir $(1)) && python setup.py -q test)

endef

testall:
	$(foreach p,$(wildcard */setup.py),$(call testT,$(p)))
