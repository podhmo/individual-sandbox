TARGET := data.har
OPTS ?=--debug
default: 00 01 02 03


# shape
00:
	dictknife ${OPTS} shape --with-type ${TARGET} | tee 00shape.txt
# schema
01:
	swaggerknife ${OPTS} json2swagger --without-example --name Top ${TARGET} --dst 01schema.yaml
# schema-unref
02:
	swaggerknife ${OPTS} json2swagger --without-example --name Top ${TARGET} -o yaml | jsonknife select --ref "#/definitions/Top@#/definitions/Top" -i yaml > 02schema.yaml

# schema-unref -> example
03:
	swaggerknife ${OPTS} json2swagger --name Top ${TARGET} -o yaml | jsonknife select --ref "#/definitions/Top@#/definitions/Top" -i yaml | jsonknife examples -f yaml --ref "#/definitions/Top" > 03examples.yaml

# shrink
04:
	python shrink.py ${TARGET} > 04data.shrink.yaml
