default:
	dictknife concat ../example_goconfig/swagger.json > swagger.yaml
	jsonknife deref --src swagger.yaml --ref '#/definitions/conf' | jsonknife samplevalue > config.json

play:
	cat config.json | go run handmade/*.go
