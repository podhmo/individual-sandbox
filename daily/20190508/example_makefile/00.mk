MESSAGE ?= hello world

default:
	@echo ${MESSAGE}

run:
	make -f 00.mk
	make -f 00.mk MESSAGE="bye bye"
