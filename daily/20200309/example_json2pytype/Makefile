00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py)

02: DATA ?= data/config.json
02:
	python json_to_x.py scan $(DATA)

03: DATA ?= data/config2.json
03:
	python json_to_x.py scan $(DATA)
test:
	pytest -s .
