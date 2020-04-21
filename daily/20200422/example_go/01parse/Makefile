SHELL := $(shell which bash)

test:
	go test -v

readme: OUTPUT ?= README.md
readme:
	( echo '```'; $(MAKE) test; echo '```' ) |& tee README.md
