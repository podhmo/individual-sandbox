default:
	python toswagger.py sample.yaml > swagger.yaml
	swagger-marshmallow-codegen swagger.yaml > schema.py

duplicated:
	python toswagger.py duplicated/duplicated.yaml | tee duplicated/swagger.yaml

.PHONY: duplicated
