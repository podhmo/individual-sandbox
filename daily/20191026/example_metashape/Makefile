00:
	python -m metashape.outputs.jsonschema --aggressive $@*.py
01:
	python -m metashape.inputs.python data/person.json
02:
	(cd foo && poetry run json2models -m User ../data/person.json) | tee 02user.py
03: data/swagger.json
	(cd foo && poetry run json2models -m OpenAPI ../data/swagger.json) | tee 03openapi.py
04: data/swagger.json
	(cd foo && poetry run json2models -m OpenAPI ../data/swagger.json -s flat) | tee 04openapi.py

data/swagger.json:
	wget https://raw.githubusercontent.com/bogdandm/json2python-models/master/testing_tools/swagger.json -O $@
