default:
	go run bin/dump/main.go ./swagger.json "#/definitions/bridge"

setup:
	go get -v github.com/lestrrat/go-jsref
	go get -v github.com/lestrrat/go-jsval/cmd/jsval
	go get -v github.com/go-openapi/jsonreference
	go get -v github.com/k0kubun/pp

jsonschema.json:
	dictknife transform --src swagger.json --code 'lambda d: [d.update(d["definitions"].pop("conf")), d][-1]' --dst jsonschema.json

jsval: jsonschema.json
	jsval -s jsonschema.json > validate.go

