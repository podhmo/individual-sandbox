define fooT
foo:
	echo $(1) yyyy
endef

$(eval $(call fooT,hello))
