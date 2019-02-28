genschema: schemas
	swaggerknife json2swagger data/fullset.json --dst schemas/parts/base.yaml
	dictknife cat schemas/parts/base.yaml schemas/parts/extend.yaml --dst schemas/schema.yaml

schemas:
	mkdir -p schemas/parts
