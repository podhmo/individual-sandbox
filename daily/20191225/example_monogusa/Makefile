00:
	TARGET=$@ $(MAKE) gen
	python $(shell echo $@*/)main.py hello --name world

gen: TARGET ?= xx
gen:
	python $(shell echo $(TARGET)*/)codegen.py | tee $(shell echo $(TARGET)*/)main.py

