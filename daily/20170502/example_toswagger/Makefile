default:
	python toswagger.py --show-minimap sample.yaml > swagger.yaml
	swagger-marshmallow-codegen swagger.yaml > schema.py

duplicated:
	python toswagger.py --show-minimap duplicated/duplicated.yaml | tee duplicated/swagger.yaml
	python toswagger.py --show-minimap duplicated/duplicated.yaml --annotations ./duplicated/annotations.yaml | tee duplicated/swagger2.yaml
	diff -u duplicated/swagger.yaml duplicated/swagger2.yaml

.PHONY: duplicated
