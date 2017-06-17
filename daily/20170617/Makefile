# need https://github.com/podhmo/{goaway,dictknife}

default:
	mkdir -p dst config
	swaggerknife json2swagger --name=config config.yaml > dst/config-spec.yaml
	swagger2go dst/config-spec.yaml --position=config --file=config.go --ref="#/definitions/config"
	goimports -w config/*.go
