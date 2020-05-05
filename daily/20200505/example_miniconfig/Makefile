export DEBUG=1
SHELL := $(shell which bash)

default: 00 01

00:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.output,$(shell echo $@*.py))
01:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.output,$(shell echo $@*.py)) | exit 0
