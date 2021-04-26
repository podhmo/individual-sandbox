00:
	python $(shell echo $@*.py) --name world
01:
	FAKE_CALL=1 python $(shell echo $@*.py) --name world
02:
	python $(shell echo $@*.py) --name world
	typer $(shell echo $@*.py) utils docs > $@hello-doc.md
03:
	python $(shell echo $@*.py) --name world
	python $(shell echo $@*.py) --simple --expose > exposed.py
	# python $(shell echo $@*.py) --expose > exposed.py
