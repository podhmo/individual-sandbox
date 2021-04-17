00:
	python $(shell echo $@*.py) foo
	python $(shell echo $@*.py) --help : | tee $@.help

01:
	python $(shell echo $@*.py) --help : | tee $@.help
	python $(shell echo $@*.py) hello --help :  | tee $@.hello.help
	python $(shell echo $@*.py) goodbye --help :  | tee $@.goobye.help

02:
	python $(shell echo $@*.py) --help : | tee $@.help
	python $(shell echo $@*.py) create --help :  | tee $@.create.help
	python $(shell echo $@*.py) delete --help :  | tee $@.delete.help
	python $(shell echo $@*.py) delete-all --help :  | tee $@.delete-all.help
	python $(shell echo $@*.py) init --help :  | tee $@.init.help

03:
	pip install typer-cli
	typer $(shell echo $@*.py) utils docs | tee $@-doc.md

04:
	typer $(shell echo $@*.py) utils docs | tee $@-doc.md
