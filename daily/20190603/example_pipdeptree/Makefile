VENV ?= venv/foo
PIP := ./${VENV}/bin/pip

default: clean
	mkdir -p ${VENV}
	python -m venv ${VENV}
	${PIP} install -r requirements.txt
	$(MAKE) all-requirements pyramid-requirements flask-requirements show-deps
	$(MAKE) diff

clean:
	rm -rf ${VENV}
	rm -f *.lock *.deps

diff:
	diff -u requirements.lock pyramid-requirements.lock > pyramid.diff || echo ok
	diff -u requirements.lock flask-requirements.lock > flask.diff || echo ok

all-requirements:
	${VENV}/bin/pip freeze | sed 's/^ *//g' | sort -u | tee requirements.lock
pyramid-requirements:
	${VENV}/bin/pipdeptree -p pyramid,mako,sqlalchemy -f | sed 's/^ *//g' | sort -u | tee pyramid-requirements.lock
flask-requirements:
	${VENV}/bin/pipdeptree -p flask,mako,sqlalchemy -f | sed 's/^ *//g' | sort -u | tee flask-requirements.lock

show-deps:
	${VENV}/bin/pipdeptree | tee all.deps
