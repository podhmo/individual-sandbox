default: 00 01 02 03 04

00:
	python 00*.py
01:
	python 01*.py
02:
	python 02*.py
03:
	python 03*.py
04:
	python 04*.py

prof: X ?= 1000
prof:
	X=${X} kernprof -l latest.py latest.py.lprof
	python -m line_profiler latest.py.lprof |& tee prof.out
