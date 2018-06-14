define fooT
foo:
	echo $(1) yyyy
endef

$(call fooT,hello)
