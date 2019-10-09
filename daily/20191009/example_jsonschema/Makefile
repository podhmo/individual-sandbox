SCHEMA ?= schema.yaml
DATA ?= petstore-expanded.yaml
CACHE ?= schema.pickle

00: download
	time python $@*.py --schema=$(SCHEMA) --data=$(DATA)

# profile
01: download
	time python -m cProfile -s cumulative $@*.py --schema=$(SCHEMA) --data=$(DATA) 2>&1 | tee 01profile.out

02: download
	time python $@*.py --schema=$(SCHEMA) --data=$(DATA) --cache=$(CACHE)

download: \
 schema.yaml\
 petstore-expanded.yaml

schema.yaml:
	wget https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/schemas/v3.0/schema.yaml

petstore-expanded.yaml:
	wget https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v3.0/petstore-expanded.yaml
