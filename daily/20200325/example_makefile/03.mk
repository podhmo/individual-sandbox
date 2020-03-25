default: foo bar

foo: base
	$(info start $@)
bar: base
	$(info start $@)

base:
	$(info start $@)

.PHONY: foo bar

