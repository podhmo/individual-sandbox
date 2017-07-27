%.output: %.py
	python $^ > $@

default:
	$(MAKE) gen -B

gen: $(shell ls *.py | sed 's@\.py$$@.output@g;')
