00:
	python 00*.py | tee 00.output
01:
	python 01*.py | tee 01.output
02:
	python 02*.py | tee 02.output
03:
	python 03*.py | tee 03.output
05:
	python 05*.py | tee 05.output
06:
	python 06*.py | tee 06.output
07:
	python 07*.py | tee 07.output
malloc:
	PYTHONMALLOC=pymalloc_debug python 04*.py 2>&1 | tee 04.output
	# PYTHONMALLOCSTATS=1 PYTHONMALLOC=pymalloc_debug python 04*.py 2>&1 | tee 04.output
