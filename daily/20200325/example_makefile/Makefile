SHELL := $(shell which bash)

00:
	$(MAKE) -f $@.mk $(MFLAGS)
01:
	$(MAKE) -f $@.mk
02:
	$(MAKE) -f $@.mk
03:
	$(MAKE) -f $@.mk
	@echo ----------------------------------------
	$(MAKE) -f $@.mk foo
	@echo ----------------------------------------
04:
	$(MAKE) -f $@.mk
	@echo ----------------------------------------
	FOO=20 BAR=20 $(MAKE) -f $@.mk
	@echo ----------------------------------------
	FOO=20 BAR=20 $(MAKE) -f $@.mk -e
05:
	$(MAKE) -f $@.mk || exit 0
	@echo ----------------------------------------
	$(MAKE) -f <(cat $@.mk dummies.mk) || exit 0
	@echo ----------------------------------------
	$(MAKE) -f <(cat $@.mk <(printf "base:\n\t:\n")) || exit 0
