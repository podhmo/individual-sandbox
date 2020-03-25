default: foo bar boo

foo:
	$(info start $@)
	$(info end $@)

bar:
	$(info start $@)
	$(info end $@)

boo:
	$(info start $@)
	$(info end $@)
