xs := foo bar boo

define doT =
	echo $(1)

endef

default:
	$(foreach x,$(xs),$(call doT,$(x)))

default2:
	$(foreach x,$(wildcard src/*.txt),$(call doT,$(x)))
