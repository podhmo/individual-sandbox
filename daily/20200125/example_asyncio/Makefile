00:
	LOGGING_FORMAT="%(levelname)s	%(relativeCreated).2f	%(message)s" DEBUG=1 python $(shell echo $@*.py)
01:
	LOGGING_FORMAT="%(levelname)s	%(relativeCreated).2f	%(message)s" DEBUG=1 python $(shell echo $@*.py)
02:
	LOGGING_FORMAT="%(process)s	%(relativeCreated).2f	%(levelname)s	%(message)s" DEBUG=1 python $(shell echo $@*.py) run

clean:
	rm -f *.fifo
