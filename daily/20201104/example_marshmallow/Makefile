00:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
	python -m swagger_marshmallow_codegen --strict-additional-properties $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
