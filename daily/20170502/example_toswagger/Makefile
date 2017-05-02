default:
	python toswagger.py sample.yaml > swagger.yaml
	swagger-marshmallow-codegen swagger.yaml > schema.py

duplicated:
	python toswagger.py duplicated/duplicated.yaml | tee duplicated/swagger.yaml
	python toswagger.py duplicated/duplicated.yaml --annotations ./duplicated/annotations.yaml | tee duplicated/swagger2.yaml
	diff -u duplicated/swagger.yaml duplicated/swagger2.yaml

.PHONY: duplicated
