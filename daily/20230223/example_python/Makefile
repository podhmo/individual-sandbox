VENVBIN ?= ./venv
PIP ?= $(VENVBIN)/bin/pip
PYTHON ?= $(VENVBIN)/bin/python
PYTEST ?= $(VENVBIN)/bin/pytest

00:
	$(PYTHON) $(shell echo $@*.py)
01:
	$(PYTHON) $(shell echo $@*.py)
02:
	$(PYTEST) $(shell echo $@*.py)



# setup: 
setup:
	$(PIP) install --upgrade pip
	$(PIP) install playwright
	$(VENVBIN)/playwright install

setup2:
	$(PIP) install pytest-playwright

