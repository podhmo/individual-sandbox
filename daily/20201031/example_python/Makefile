PORT ?= 8888
00:
	uvicorn $(patsubst %.py,%,$(shell echo $@*.py)):app --port $(PORT)
01:
	uvicorn $(patsubst %.py,%,$(shell echo $@*.py)):app --port $(PORT)
02:
	python $(shell echo $@*.py)

# transform
03:
	time $(MAKE) -C ../../20201029/example_python 02

# transform via api
04:
	uvicorn $(patsubst %.py,%,$(shell echo $@*.py)):app --port $(PORT)
04-client:
	echo '{"filename": "data/iris/species-sepalLength.json"}' | http --json post :8888/dataset/iris/groupby/species/aggs/sepalLength fn==min fn==max fn==mean fn==std fn==count
	echo '{"filename": "data/iris/species-sepalWidth.json"}' | http --json post :8888/dataset/iris/groupby/species/aggs/sepalWidth fn==min fn==max fn==mean fn==std fn==count
	echo '{"filename": "data/cars/Year-Horsepower.json"}' | http --json post :8888/dataset/cars/groupby/Year/aggs/Horsepower fn==min fn==max fn==mean fn==std fn==count

05:
	DEBUG=1 time python $(shell echo $@*.py)

06: PYTHON ?= python
06:
	$(PYTHON) $(shell echo $@*.py)
07: PYTHON ?= python3.9
07:
	$(PYTHON) $(shell echo $@*.py)

