# single command
00:
	python $(shell echo $@*.py) --help
	python $(shell echo $@*.py) world

# sub commands
01:
	python $(shell echo $@*.py) --help
	python $(shell echo $@*.py) hello --help
	python $(shell echo $@*.py) goodbye --help
	python $(shell echo $@*.py) hello world
	python $(shell echo $@*.py) goodbye world
02:
	python $(shell echo $@*.py) --help
	python $(shell echo $@*.py) hello --help
	python $(shell echo $@*.py) goodbye --help
	python $(shell echo $@*.py) hello world
	python $(shell echo $@*.py) goodbye world

setup:
	pip install -r requirements.txt
