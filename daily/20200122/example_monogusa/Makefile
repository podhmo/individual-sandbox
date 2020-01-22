00:
	python multicommands.py hello
	python multicommands.py bye
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py) manager
02:
	python $(shell echo $@*.py) manager
03:
	DEBUG=1 python $(shell echo $@*.py) manager
04:
	DEBUG=1 python $(shell echo $@*.py) manager
05:
	DEBUG=1 python $(shell echo $@*.py) manager
06:
	DEBUG=1 python $(shell echo $@*.py) manager
07:
	LOGGING_FORMAT="%(process)d	%(message)s" DEBUG=1 python $(shell echo $@*.py) manager
