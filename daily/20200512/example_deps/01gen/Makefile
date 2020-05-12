default: dst/x.yaml dst/x.go dst/y.go

dst/x.yaml: _tools/gen.py _tools/x-yaml.j2 x.csv
	python $< x-yaml _tools/x-yaml.j2 x.csv > $@
dst/x.go: _tools/gen.py _tools/x-go.j2 x.csv
	python $< x-go _tools/x-go.j2 x.csv > $@

dst/y.go: _tools/gen.py _tools/y-go.j2 root.yaml
	python $< y-go _tools/y-go.j2 root.yaml > $@

include: _setup.mk

clean:
	rm -rf _tools dst
	mkdir -p _tools dst
