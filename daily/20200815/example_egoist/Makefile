00:
	python $(shell echo $@*/)definitions.py

01: OPTS ?= --rootdir=.
01:
	python cli.py $(OPTS) $(shell echo $@*/)definitions.py

02:
	egoist-cli $(shell echo $@*/)definitions.py
03:
	python gen.py $(OPTS) $(shell echo $@*/)*.py
04:
	python gen.py $(OPTS) $(shell echo $@*/)*.py
05:
	python gen.py $(OPTS) $(shell echo $@*/)*.py
