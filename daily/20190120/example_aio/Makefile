PYTHON ?= LOGLEVEL=DEBUG PYTHONASYNCIODEBUG=1 python -W default

00:
	time ${PYTHON} 00*.py
01:
	time ${PYTHON} 01*.py
02:
	time ${PYTHON} 02*.py
03:
	time ${PYTHON} 03*.py
04:
	time ${PYTHON} 04*.py
05:
	time ${PYTHON} 05*.py
06:
	time ${PYTHON} 06*.py
07:
	time ${PYTHON} 07*.py
diff:
	diff -u 00*.py 01*.py | tee 00to01.diff
