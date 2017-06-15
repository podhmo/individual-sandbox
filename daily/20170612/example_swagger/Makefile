%.py: %.yaml
	swagger-marshmallow-codegen --full $< > $@

default:  $(shell ls *.yaml | sed 's/\.yaml$$/.py/g')
