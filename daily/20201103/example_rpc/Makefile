PORT ?= 8888
export DEBUG = 1

SENTINEL ?= xxx

00:
	python $(shell echo $@*.py) --port $(PORT)
x00:
	python $(shell echo $@*.py) --port $(PORT)
01:
	python $(shell echo $@*.py) --port $(PORT) --sentinel=$(SENTINEL)
x01:
	python $(shell echo $@*.py) --port $(PORT) --sentinel=$(SENTINEL)

# use
02:
	python $(shell echo $@*.py)
03:
	python $(shell echo $@*.py) --port $(PORT) --sentinel=$(SENTINEL)
x03:
	python $(shell echo $@*.py)

# echo python $(shell echo $(shell echo $@ | gsed 's/^x//')*.py)
