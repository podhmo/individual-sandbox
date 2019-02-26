BIN ?= ./node_modules/.bin

setup:
	npm install speccy swagger2openapi

2to3:
	${BIN}/swagger2openapi -y -v -r -o 3.0.yaml 2.0.yaml

lint:
	${BIN}/speccy lint 3.0.yaml

serve:
	${BIN}/speccy serve 3.0.yaml
