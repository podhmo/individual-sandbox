test:
	python setup.py test

format:
#	pip install -e .[dev]
	black foo setup.py

lint:
#	pip install -e .[dev]
	flake8 foo --ignore W503,E203,E501

build:
#	pip install wheel
	python setup.py bdist_wheel

upload:
#	pip install twine
	twine check dist/foo-$(shell cat VERSION)*
	twine upload dist/foo-$(shell cat VERSION)*

.PHONY: test format lint build upload
