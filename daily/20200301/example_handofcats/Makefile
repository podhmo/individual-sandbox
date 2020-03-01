00:
	DEBUG=1 python $(shell echo $@*.py)
01:
	LOGGING_TIME=relative DEBUG=1 python $(shell echo $@*.py)
02:
	LOGGING_TIME=relative DEBUG=1 python $(shell echo $@*.py)
03:
	diff -u 01*.py 02*.py > 0102.diff
