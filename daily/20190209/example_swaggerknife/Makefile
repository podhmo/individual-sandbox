default: schema
	swaggerknife json2swagger config/db.json --name db --dst schema/db.json
	swaggerknife json2swagger config/flags.json --name flags --dst schema/flags.json
	swaggerknife json2swagger config/api.json --name api --dst schema/api.json
	swaggerknife merge --wrap config --style ref `ls schema/*.json | grep -v "schema/main[0-9]*.json"` --dst schema/main.json
	jsonknife bundle --src schema/main.json --dst bundle.json
schema:
	mkdir -p schema
