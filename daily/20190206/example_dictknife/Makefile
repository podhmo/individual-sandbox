OPTS := --debug

default: 00 01

00: src/person.json src/db.json
01: src/main.json

src:
	mkdir -p src
src/main.json:
	swaggerknife ${OPTS} merge

src/person.json: src
	swaggerknife ${OPTS} json2swagger data/00/*.json --name person --dst $@ --annotate data/annotation/00.json
# todo: shared
src/db.json: src
	swaggerknife ${OPTS} json2swagger data/01/*.json --name db --dst $@ #--annotate data/annotation/01.json
