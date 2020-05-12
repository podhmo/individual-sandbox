default: dst/x.yaml dst/x.go dst/y.go

dst/x.yaml: _tools/x-yaml.py _tools/x-yaml.j2 x.csv
	python $^ > $@
dst/x.go: _tools/x-go.py _tools/x-go.j2 x.csv
	python $^ > $@

dst/y.go: _tools/y-go.py _tools/y-go.j2 root.yaml
	python $^ > $@

_tools/x-yaml.py:
	echo 'import sys; print(__file__, sys.argv[1:])' > $@
_tools/x-go.py:
	echo 'import sys; print(__file__, sys.argv[1:])' > $@
_tools/y-go.py:
	echo 'import sys; print(__file__, sys.argv[1:])' > $@

_tools/x-yaml.j2:
	echo $@ > $@
_tools/x-go.j2:
	echo $@ > $@

_tools/y-go.j2:
	echo $@ > $@

x.csv:
	echo $@ > $@
root.yaml:
	echo $@ > $@

clean:
	rm -rf _tools dst
	mkdir -p _tools dst
