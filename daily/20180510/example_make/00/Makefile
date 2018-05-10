define F =
	echo $(1)
	echo $(1)

endef

xs := foo bar
f2:
	$(foreach x,$(xs),$(call F,$(x)))

f0:
	$(call F,foo)
	$(call F,bar)

