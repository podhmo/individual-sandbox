PY_FILE = $(shell echo $@*.py)

00:
	mypy --strict $(PY_FILE)
01:
	mypy --strict $(PY_FILE)
02:
	diff -u 01*.py 02*.py || exit 0
	mypy --strict $(PY_FILE)

setup:
	python -m pip install -r requirements.txt

