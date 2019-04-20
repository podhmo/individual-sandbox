MSG ?= "hello"

hello:
	@echo ${MSG}
	echo ${MSG}

bye:
	echo $(shell echo bye)

.DEFAULT_GOAL = bye
