00:
	python $(shell echo $@*.py)
01:
	python -m prestring.python hello.py
	python hello.py
	python <(python <(python -m prestring.python hello.py))
