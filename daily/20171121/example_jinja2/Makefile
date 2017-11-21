VERSION ?= 2.10

run:
	VERSION=2.9.6 $(MAKE) checktime
	$(MAKE) checktime

checktime:
	(pip install --force-reinstall Jinja2==${VERSION} || exit 0)
	time python -c 'import jinja2'
	time python -c 'import jinja2'
	time python -c 'import jinja2'
