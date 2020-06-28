FILE ?= petstore-minimal.yaml

00:
	python $(shell echo $@*.py) --filename $(FILE)
01:
	python $(shell echo $@*.py) --filename $(FILE)
02: FILE := petstore.yaml
02:
	python $(shell echo $@*.py) --filename $(FILE)
03: FILE := person.yaml
03:
	python $(shell echo $@*.py) --filename $(FILE)

petstore-minimal.yaml:
	wget https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v2.0/yaml/petstore-minimal.yaml
petstore.yaml:
	wget https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v2.0/yaml/petstore.yaml
