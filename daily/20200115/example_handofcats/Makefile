00:
	DEBUG=1	python -m handofcats $(shell echo $@*.py) hello
	python -m handofcats $(shell echo $@*.py) --expose > gen-$(shell echo $@*.py).py
	DEBUG=1 python gen-$(shell echo $@*.py).py hello
01:
	python -X importtime $(shell echo $@*.py)
