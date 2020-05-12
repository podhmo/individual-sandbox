# goal task
DEP ?= dst/x.yaml dst/x.go dst/y.go
PRE ?= pre/dst/x.yaml.py pre/dst/x.go.py pre/dst/y.go.py
CONT ?= PRE=$< DEP="" $(MAKE) _default

default:
	CONT="exit 0" $(MAKE) _gen
_gen:
_gen: $(DEP)
	echo '**' $(PRE) '**' > /dev/stderr
	{ $(foreach p,$(PRE),cat $(p);) } | bash -x

pre/dst/x.yaml.py: _tools/x-yaml.j2 x.csv
	echo "python _tools/gen.py $(subst .,-,$(basename $(@F))) $^ > $(subst pre/,,$(patsubst %.py,%,$@))" > $@
pre/dst/x.go.py:  _tools/x-go.j2 x.csv
	echo "python _tools/gen.py $(subst .,-,$(basename $(@F))) $^ > $(subst pre/,,$(patsubst %.py,%,$@))" > $@

pre/dst/y.go.py: _tools/y-go.j2 root.yaml
	echo "python _tools/gen.py $(subst .,-,$(basename $(@F))) $^ > $(subst pre/,,$(patsubst %.py,%,$@))" > $@

dst/x.yaml: pre/dst/x.yaml.py _tools/gen.py
	$(CONT)
dst/x.go: pre/dst/x.go.py _tools/gen.py
	$(CONT)
dst/y.go: pre/dst/y.go.py _tools/gen.py
	$(CONT)

include _setup.mk

clean:
	rm -rf _tools dst pre
	mkdir -p _tools dst pre/dst
