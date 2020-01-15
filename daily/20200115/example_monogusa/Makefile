00:
	# python -m monogusa.cli $(shell echo $@*.py) hello
	DEBUG=1 python -m monogusa.cli $(shell echo $@*.py) hello
	LOGGING_TIME=relative DEBUG=1 python -m monogusa.cli $(shell echo $@*.py) hello	
01:
	python $(shell echo $@*.py) hello
	DEBUG=1 python $(shell echo $@*.py) hello
	LOGGING_TIME=relative DEBUG=1 python $(shell echo $@*.py) hello
