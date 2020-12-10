SHELL := bash

00:
	python use.py --config <(python config.py)

check:
	mypy --strict config.py

config:
	python config.py
.PHONY: config
