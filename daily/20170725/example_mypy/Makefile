# OPTS = -v --tb --script
OPTS = --tb --script

%.output:%.py
	bash -c 'mypy ${OPTS} $< 2>&1' > $@ || exit 0

default: $(shell ls *.py | sed 's/\.py$$/.output/')
clean:
	rm *.output
