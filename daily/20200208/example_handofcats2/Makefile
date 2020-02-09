00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py)
02:
	python $(shell echo $@*.py)
03:
	python -m handofcats $(shell echo $@*.py) people --cont=$(shell echo $@*.py):_output
diff:
	python 00*.py --expose  > output.full.py
	python 00*.py --expose --simple > output.simple.py
	diff -u output.full.py output.simple.py
