SHELL := bash

default:
	invoke clean build

dry-run:
	invoke --dry clean build

help:
	invoke --help |& tee help.txt || :
	invoke build --help |& tee help.build.txt || :
	invoke clean --help |& tee help.clean.txt || :
