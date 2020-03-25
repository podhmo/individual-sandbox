default: foo bar boo

foo:
	$(info start $@)
	$(info end $@)
bar:
	$(info start $@)
	exit 1
	$(info end $@)
boo:
	$(info start $@)
	$(info end $@)
