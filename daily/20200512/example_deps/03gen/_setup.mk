
_tools/gen.py:
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
