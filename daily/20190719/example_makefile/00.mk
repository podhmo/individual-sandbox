ifdef production
	OPTION = -o
else
	OPTION = --debug
endif

$(info load 00.mk)

00:
	$(MAKE) _run00
	$(MAKE) _run00 production=1

_run00:
	echo @$(OPTION)@
