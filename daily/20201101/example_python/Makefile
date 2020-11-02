SHELL := bash
PORT ?= 8888

00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py)
02:
	python $(shell echo $@*/main.py)
03:
	python $(shell echo $@*.py)
04:
	python -m swagger_marshmallow_codegen  $(shell echo $@*.yaml)

# echo '{"name": "foo"}' | http --json POST :8888
05:
	uvicorn $(patsubst %.py,%,$(shell echo $@*.py)):app --port $(PORT)
x05:
	DEBUG=1 python $(shell echo $@*.py) --port $(PORT)
07:
	python $(shell echo $@*.py) --port $(PORT)
