00:
	python 00*.py
01:
	python 01*.py
02:
	python 02*.py
03:
	mypy --strict 03*.py
	python 03*.py

gen-lock:
	cat requirements.txt | pipdeptidy > requirements.lock
