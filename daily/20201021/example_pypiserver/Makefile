SHELL := bash

PYTHON ?= python
PORT ?= 7000
PACKAGE_DIR := pypi/packages
REPOSITORY_URL ?= http://localhost:$(PORT)

TARGET_PACKAGE ?= sample

# pypi-server
start: bin/pypi-server
	bin/pypi-server \
 -vvv \
 -P . -a . \
 -p $(PORT) $(PACKAGE_DIR)

bin/python:
	$(PYTHON) -m venv .
bin/pypi-server: requirements.txt bin/python
	bin/python -m pip install -r $<


## create-package
create-package:
# require pip install -r requirements-user.txt
	cookiecutter gh:podhmo/pypackage


# upload package
upload-sample: pkg = dist/sample-$(shell cat $(TARGET_DIR)/VERSION)*.whl
upload-sample: $(TARGET_DIR)
	cd $(TARGET_DIR) && \
 make build && \
 twine check $(pkg) && \
 twine upload --repository-url $(REPOSITORY_URL) $(pkg) && \
 :
.PHONY: upload-sample

# register
register:
	twine register --repository-url $(REPOSITORY_URL)
.PHONY: register

## fresh
install-sample: foo
	foo/bin/python -m pip install \
 --index-url $(REPOSITORY_URL)/simple \
 --extra-index-url $(REPOSITORY_URL) \
 $(TARGET_PACKAGE)
 :

foo:
	mkdir -p $@
	python -m venv $@

clean:
	rm -rf foo
.PHONY: clean
