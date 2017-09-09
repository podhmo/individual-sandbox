%.output: %.py
	python $^ > $@

default: $(shell ls *.py | sed 's/\.py$$/.output/' )

clean: *.output
	rm -f $^
