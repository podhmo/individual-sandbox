default: 00 01 02 03 04 05

00:
	SUFFIX=$@ DIR=$@gen $(MAKE) gen NAMES="b.f"
01:
	SUFFIX=$@ DIR=$@gen $(MAKE) gen NAMES="a.f"
02:
	SUFFIX=$@ DIR=$@gen $(MAKE) gen NAMES="b.f b.g"
03:
	SUFFIX=$@ DIR=$@gen $(MAKE) gen NAMES="b.f a.g"
04:
	SUFFIX=$@ DIR=$@gen $(MAKE) gen NAMES="a.f b.g"
05:
	SUFFIX=$@ DIR=$@gen $(MAKE) gen NAMES="a.f a.g"

clean:
	rm -rf *gen
.PHONY: clean

gen: NAMES ?= b.f b.g b.h
gen: DIR ?= default
gen: SUFFIX ?= 
gen:
	mkdir -p $(DIR)
	python gen.py$(foreach x,$(NAMES), --names=$(x)) > $(DIR)/main$(SUFFIX).go
.PHONY: gen
