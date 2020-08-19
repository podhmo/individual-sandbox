PYTHON ?= bin/python

00:
	$(PYTHON) $(shell echo $@*.py)

notebook:
	bin/jupyter-notebook

setup: bin/python
	bin/python -m pip install -r requirements.txt

bin/python:
	python -m venv . --system-site-packages
