PYTHON := .venv/Scripts/python

00:
	$(PYTHON) $(shell echo $@*.py)
01:
	$(PYTHON) $(shell echo $@*.py)

.venv/Scripts/python: 
	python -m venv .venv

setup: $(PYTHON)
	$(PYTHON) -m pip install -r requirements.txt
.PHONY: setup