PYTHON ?= venv/bin/python

00:
	$(PYTHON) $(shell echo $@*.py)

01:
	$(PYTHON) $(shell echo $@*.py)
02:
	$(PYTHON) $(shell echo $@*.py)
03:
	$(PYTHON) $(shell echo $@*.py) greet
04:
	$(PYTHON) $(shell echo $@*.py)
05:
	$(PYTHON) $(shell echo $@*.py)
06:
	$(PYTHON) $(shell echo $@*.py)


install: $(PYTHON)
	$(PYTHON) -m pip install -r requirements.txt

venv/bin/python:
	python -m venv venv
	$(PYTHON) -m pip install -U pip
