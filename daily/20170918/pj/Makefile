%.md:%.py
	nbreversible -f markdown --execute $< > $@

default: sample-data $(shell ls *.py | sed 's/\.py$$/.md/g;')

sample-data:
	git clone git@github.com:practical-jupyter/sample-data.git
