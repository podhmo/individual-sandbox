00:
	python $(shell echo $@*.py)
01:
	DEBUG=1 python $(shell echo $@*.py)
02:
	python $(shell echo $@*.py)
03:
	$(MAKE) -s 02 | python $(shell echo $@*.py)
04:
	python $(shell echo $@*.py)
