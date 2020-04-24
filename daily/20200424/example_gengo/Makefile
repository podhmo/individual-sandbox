00:
	python $(shell echo $@*.py) | tee $(patsubst %.py,%.dot,$(shell echo $@*.py))
	dot -O -Tsvg $(patsubst %.py,%.dot,$(shell echo $@*.py))
