PYTHON ?= .venv/bin/python

00: $(PYTHON)
	$(PYTHON) $(shell echo $@*.py)

01: $(PYTHON)
	$(PYTHON) $(shell echo $@*.py)

02: $(PYTHON)
	$(PYTHON) $(shell echo $@*.py)

03: $(PYTHON)
	$(PYTHON) $(shell echo $@*.py)

04: $(PYTHON)
	$(PYTHON) $(shell echo $@*.py)

05: $(PYTHON)
	$(PYTHON) $(shell echo $@*.py)

06: $(PYTHON)
	$(PYTHON) -m pydoc $(patsubst %.py,%,$(shell echo $@*.py))


.venv/bin/python:
	python -m venv .venv
	.venv/bin/pip install -U pip wheel
	$(MAKE) install

install: $(PYTHON)
	$(PYTHON) -m pip install -r requirements.txt
.PHONY: install
