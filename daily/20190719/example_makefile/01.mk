ifeq ($(env),dev)
	SUFFIX = -dev.json
else
	SUFFIX = .json
endif

$(info load 01.mk $(SUFFIX))

01:
	$(MAKE) _run01
	$(MAKE) _run01 env=dev

_run01:
	echo config$(SUFFIX)
