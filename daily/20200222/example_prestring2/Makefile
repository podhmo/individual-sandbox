00:
	python $(shell echo $@*.py) | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
#	echo python '<(python $(shell echo $@*.py))'

01:
	python -m prestring.python $(shell echo $@*.py) | tee $(patsubst %.py,%.output,$(shell echo $@*.py))

02:
	python $(shell echo $@*.py)
03:
	python -m prestring.python aexecutor.py | tee $@aexecutor.py
04:
	python -m prestring.python.parse aexecutor.py | tee $@aexecutor.output
