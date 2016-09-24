%.py.out: %.py
	python $< $2>&1 | sed "s@$$HOME@~@g" > $@

default: readme.md

readme.md: $(shell ls | grep '[0-9][0-9]*.*\.py$$' | sed 's@$$@.out@g')
	touch tmp
	for i in $^; do echo "## $$i" >> tmp; echo '```' >> tmp; cat $$i >> tmp; echo '```' >> tmp; done
	mv tmp $@

clean:
	rm -f *.py.out
	rm -f readme.md

.PHONY: default clean
