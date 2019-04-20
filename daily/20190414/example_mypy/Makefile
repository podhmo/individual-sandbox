setup:
	pip install --upgrade mypy mypy_extensions typing_extensions
	pip install -e foo

MYPY := MYPYPATH=foo:bar mypy

00:
	${MYPY} 00*.py
01:
	${MYPY} 01*.py
02:
	${MYPY} 02*.py
03:
	${MYPY} 03*.py
04:
	${MYPY} 04*.py

# orm
# https://github.com/sdispater/orator
# pip install orator
