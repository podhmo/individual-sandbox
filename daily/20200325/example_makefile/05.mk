base:
	$(info start $@)
.PHONY: base

foo: base
	$(info start $@)
.PHONY: foo

.DEFAULT_GOAL := foo
