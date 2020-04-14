SHELL := $(shell which bash)

00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.svg,$(shell echo $@*.py))


setup:
	python -m pip install --no-deps vega_datasets
