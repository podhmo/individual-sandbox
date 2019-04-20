hello:
	echo hello
	echo hello

bye:
	echo $(shell echo bye)

.DEFAULT_GOAL = bye
