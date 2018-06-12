foo := xxx yyy zzz

define echoT
$(1):
	echo $(2) $(1)

endef

default: $(foo)
$(foreach x,$(foo),$(eval $(call echoT,$(x),hello)))

