SHELL := $(shell which bash)

# check sh
# $(info $(SHELL))

00:
	python hello.py

01:
	python -m prestring.python hello.py

02:
	python <(python -m prestring.python hello.py)

03:
	python <(python <(python -m prestring.python hello.py))
