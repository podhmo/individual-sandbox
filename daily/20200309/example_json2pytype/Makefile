00: output
	python $(shell echo $@*.py) | tee output/$@
01: output
	python $(shell echo $@*.py) | tee output/$@

02: DATA ?= data/config.json
02: output
	python json_to_x.py scan $(DATA) | tee output/$@

03: DATA ?= data/config2.json
03: output
	python json_to_x.py scan $(DATA) | tee output/$@

04: DATA ?= data/config2.json
04: output
	python json_to_x.py emit $(DATA) | tee output/$@

05: DATA ?= data/name-conflict.json
05: output
	python json_to_x.py emit $(DATA) | tee output/$@
	python json_to_x.py annotate $(DATA) | tee output/$@.annotate
	python json_to_x.py annotate $(DATA) --use-fullname | tee output/$@.annotate.fullname
06: DATA ?= ../../../../json2python-models/testing_tools/swagger.json
06: DATA ?= ../../../../json2python-models/testing_tools/large_data_set.json
06: output
	python json_to_x.py emit $(DATA) | tee output/$@
	# python json_to_x.py annotate $(DATA)

output:
	mkdir -p $@

test:
	pytest -s .
