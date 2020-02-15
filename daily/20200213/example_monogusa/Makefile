# all in one
00:
	python $(shell echo $@*.py) run

# use defaultcomponent
01:
	python $(shell echo $@*.py) run

# use defaultcomponent, but invalid name
02:
	python $(shell echo $@*.py) ok
	python $(shell echo $@*.py) ng


03:
	python $(shell echo $@*.py)

# must be error
04:
	python $(shell echo $@*.py) use
