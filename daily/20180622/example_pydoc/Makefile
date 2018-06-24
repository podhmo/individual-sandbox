dist/%.out: %.py
	mkdir -p dist
	python $^ | sed 's@${HOME}@$$HOME@g'| tee $@

default: $(addprefix dist/,$(addsuffix .out,$(basename $(wildcard *.py))))

clean:
	rm -rf dist
