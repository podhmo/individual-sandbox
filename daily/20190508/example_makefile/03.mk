SED := $(shell which gsed 2>/dev/null || which sed)

default:
	@echo foo | ${SED} 's/o/@/g'
