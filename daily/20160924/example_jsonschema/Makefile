%.py.err.txt: %.py
	python $< 2>&1 | sed "s@$$HOME@~@g" > $@

default: $(shell find . -name "*check.py" | sed 's@$$@.err.txt@g')

clean:
	rm -f *.err.txt

.PHONY: clean default
