00:
	jsonschema -i $@*/person.json $@*/schema.json
01:
	python -m metashape.outputs.jsonschema --aggressive $@*/person.py | tee $$(echo $@*/)schema.json
