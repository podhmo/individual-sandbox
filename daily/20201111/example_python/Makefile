PORT ?= 8888
export DEBUG := 1
00:
	python $(shell echo $@*.py) --port $(PORT)
x00:
	python -m uvicorn $(patsubst %.py,%,$(shell echo $@*.py)):app --port $(PORT)

01:
	python $(shell echo $@*.py) --port $(PORT)
02:
	python $(shell echo $@*.py)
03:
	python $(shell echo $@*.py)
04:
	python $(shell echo $@*.py)
05:
	python $(shell echo $@*.py)
