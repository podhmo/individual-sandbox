SHELL := $(shell which bash)

default: 00 01 02 03

00:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.go,$(shell echo $@*.py))
01:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.go,$(shell echo $@*.py))
02:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.go,$(shell echo $@*.py))
03:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.go,$(shell echo $@*.py))
