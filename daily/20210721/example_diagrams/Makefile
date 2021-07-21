PYTHON ?= .venv/Scripts/python.exe

00:
	$(PYTHON) $(shell echo $@*.py)
01:
	$(PYTHON) $(shell echo $@*.py)
02:
	$(PYTHON) $(shell echo $@*.py) > architecture.embed.svg
03:
	$(PYTHON) $(shell echo $@*.py)

setup: $(PYTHON)
	$(PYTHON) -m pip install diagrams

.venv/Scripts/python:
	python -m venv .venv
	$@ -m pip install wheel