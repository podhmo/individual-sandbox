00:
	jsonschema -i $@*/person.json $@*/schema.json
01:
	python -m metashape.outputs.jsonschema --aggressive $@*/person.py | tee $$(echo $@*/)schema.json
02:
	python -m metashape.outputs.jsonschema --aggressive $@*/*.py | tee $$(echo $@*/)schema.json
03:
	python $@*/main.py
04:
	mypy --strict --strict-equality $@*.py
	python $@*.py
