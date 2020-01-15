default: 00 01 02 03 04 05

00:
	python -u -X importtime $(shell echo $@*.py) 2>&1 | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
01:
	python -u -X importtime $(shell echo $@*.py) 2>&1 | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
02:
	python -u -X importtime $(shell echo $@*.py) 2>&1 | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
03:
	python -u -X importtime $(shell echo $@*.py) 2>&1 | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
04:
	time python -u -X importtime $(shell echo $@*.py) 2>&1 | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
05:
	time python -u -X importtime $(shell echo $@*.py) 2>&1 | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
