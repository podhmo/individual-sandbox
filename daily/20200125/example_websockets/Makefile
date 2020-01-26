00:
	LOGGING_STREAM=stdout LOGGING_FORMAT="%(process)s	%(levelname)s	%(message)s" DEBUG=1 python $(shell echo $@*.py) run | tee $(patsubst %.py,%.out,$(shell echo $@*.py))
