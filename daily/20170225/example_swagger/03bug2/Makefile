default:
	dictknife concat swagger.yaml --dst swagger.json
	swagger generate model -t . -f swagger.json

setup:
	go get -v github.com/go-swagger/go-swagger/cmd/swagger
	go get -v github.com/go-openapi/runtime
	go get -v github.com/tylerb/graceful
	go get -v github.com/docker/go-units
