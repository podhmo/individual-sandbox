test:
	pytest -vv --show-capture=all

ci:
	pytest --show-capture=all --cov=sample --no-cov-on-fail --cov-report term-missing
	$(MAKE) lint typing

format:
#	pip install -e .[dev]
	black sample setup.py

# https://www.flake8rules.com/rules/W503.html
# https://www.flake8rules.com/rules/E203.html
# https://www.flake8rules.com/rules/E501.html
lint:
#	pip install -e .[dev]
	flake8 sample --ignore W503,E203,E501

typing:
#	pip install -e .[dev]
	mypy --strict --strict-equality --ignore-missing-imports sample
mypy: typing

build:
#	pip install wheel
	python setup.py sdist bdist_wheel

upload:
#	pip install twine
	twine check dist/sample-$(shell cat VERSION)*
	twine upload dist/sample-$(shell cat VERSION)*

.PHONY: test ci format lint typing mypy build upload
