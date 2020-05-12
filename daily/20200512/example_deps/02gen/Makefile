# goal task
default: dst/x.yaml dst/x.go dst/y.go
	# bashでごまかしてるけれど、bulk action的に実行できるようなスクリプトを実行する(gen.pyの代わり)
	cat pre/dst/*.py | bash -x

pre/dst/x.yaml.py: _tools/x-yaml.j2 x.csv
	echo "python _tools/gen.py $(subst .,-,$(basename $(@F))) $^ > $(subst pre/,,$(patsubst %.py,%,$@))" > $@
pre/dst/x.go.py:  _tools/x-go.j2 x.csv
	echo "python _tools/gen.py $(subst .,-,$(basename $(@F))) $^ > $(subst pre/,,$(patsubst %.py,%,$@))" > $@

pre/dst/y.go.py: _tools/y-go.j2 root.yaml
	echo "python _tools/gen.py $(subst .,-,$(basename $(@F))) $^ > $(subst pre/,,$(patsubst %.py,%,$@))" > $@

dst/x.yaml: pre/dst/x.yaml.py _tools/gen.py
	touch $@ # dummy
dst/x.go: pre/dst/x.go.py _tools/gen.py
	touch $@ # dummy
dst/y.go: pre/dst/y.go.py _tools/gen.py
	touch $@ # dummy

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

clean:
	rm -rf _tools dst pre
	mkdir -p _tools dst pre/dst
