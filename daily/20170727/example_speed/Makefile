default:

00:
	kernprof -l 00base.py
	python -m line_profiler 00base.py.lprof > 00base.py.result

01:
	python 01concurrent.py

02: # contextual
	kernprof 02base.py
	python -m line_profiler 02base.py.prof
