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

04: DATA ?= data/config2.json
04:
	python json_to_x.py emit $(DATA)

05: DATA ?= data/name-conflict.json
05:
	python json_to_x.py emit $(DATA)
	python json_to_x.py annotate $(DATA)
	python json_to_x.py annotate $(DATA) --use-fullname
test:
	pytest -s .
