default: foo bar boo

foo:
	$(info start foo)
	@echo fooooooo
	$(info end foo)

bar:
	$(info start bar)
	@echo barrrrrrr
	$(info end bar)

boo:
	$(info start boo)
	@echo booooo
	$(info end boo)
