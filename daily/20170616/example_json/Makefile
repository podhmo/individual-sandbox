default:
	swaggerknife json2swagger --name config config.json > config.spec.yaml
	swaggerknife json2swagger people.yaml > person.spec.yaml
jsonschema:
	pip install jsonschema   # jsonschema コマンドが実行できるようになる
	swaggerknife json2swagger --emit=jsonschema config.json --dst schema.json
	jsonschema -i config.json schema.json
.PHONY: jsonschema
