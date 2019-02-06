OPTS := --debug

default: 00

src:
	mkdir -p src

00: src
	swaggerknife ${OPTS} json2swagger data/00/*.json --name person --annotate data/annotation/00.json --dst src/person.json
# todo: shared
