PYTHON ?= .venv/Scripts/python.exe

00:
	$(PYTHON) $(shell echo $@*.py)
01:
	$(PYTHON) $(shell echo $@*.py)
02:
	$(PYTHON) $(shell echo $@*.py)
03:
	$(PYTHON) $(shell echo $@*.py)

setup: $(PYTHON)
	$(PYTHON) -m pip install sympy

.venv/Scripts/python.exe:
	python -m venv .venv
	$@ -m pip install wheel