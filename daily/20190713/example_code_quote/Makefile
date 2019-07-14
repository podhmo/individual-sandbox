00:
	python 00*.py | tee 00.output
01:
	python 01*.py | tee 01.output

02:
	python 02*.py target00.py --lineno=4 | tee 0200.output
	python 02*.py target01.py --lineno=18 | tee 0201.output
	python 02*.py $(HOME)/venvs/my/lib/python3.7/site-packages/starlette/endpoints.py --lineno=99 | tee 0202.output

03:
	python 03*.py target01.py --lineno=18
	python 03*.py target03.py --lineno=14
	python 03*.py target01.py --lineno=29
	python 03*.py $(HOME)/venvs/my/lib/python3.7/site-packages/starlette/endpoints.py --lineno=99
	python 03*.py /usr/lib/python3.7/argparse.py --lineno=2000
