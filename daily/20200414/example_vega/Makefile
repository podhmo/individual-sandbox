SHELL := $(shell which bash)

00:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.svg,$(shell echo $@*.py))
setup:
	pip install -r requirements.txt
