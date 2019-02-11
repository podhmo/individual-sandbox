default: generate-code

generate-parts: schema
	swaggerknife json2swagger data/db*.json --name db --dst schema/db.json
	swaggerknife json2swagger data/flags.json --name flags --dst schema/flags.json
	swaggerknife json2swagger data/api.json --name api --dst schema/api.json

merge-parts: generate-parts
	swaggerknife merge --wrap config --style ref `ls schema/*.json | grep -v "schema/main[0-9]*.json"` --dst schema/main.json

generate-code: generate-parts
	jsonknife bundle --src schema/main.json --dst bundle.json
	swagger-marshmallow-codegen bundle.json > schema.py

schema:
	mkdir -p schema
