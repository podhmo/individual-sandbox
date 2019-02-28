PYTHONBRAKPOINT := pudb.set_trace

00:
	python validate.py schema.yaml

schema.json: schema.yaml
	dictknife cat schema.yaml --dst schema.json

valildate: TARGET ?= data/pass.json
validate: schema.json
	jsonschema -i ${TARGET} schema.json
01:
	$(MAKE) validate
02:
	$(MAKE) validate TARGET=data/fail_aws_invalid_region.json

setup:
	pip install jsonschema
