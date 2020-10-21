SHELL := bash

PYTHON ?= python

default: s3-pypi.json bin/s3pypi

bin/python:
	$(PYTHON) -m venv .
bin/s3pypi: requirements.txt bin/python
	bin/python -m pip install -r $<

s3-pypi.json:
	wget https://raw.githubusercontent.com/novemberfiveco/s3pypi/master/cloudformation/s3-pypi.json

