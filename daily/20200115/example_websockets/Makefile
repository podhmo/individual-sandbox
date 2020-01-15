00:
	python $(shell echo $@*.py)
01:
	for i in `seq 100`; do python $(shell echo $@*.py)& done
02:
	python $(shell echo $@*.py)
03:
	DEBUG=1 LOGGING_TIME=relative python $(shell echo $@*.py)
04:
	DEBUG=1 LOGGING_TIME=relative python $(shell echo $@*.py)
05:
	python $(shell echo $@*.py)


setup:
	pip install -r requirements.txt
