# need https://github.com/podhmo/{goaway,dictknife}
EXTRA = --detector="./resolve.py:Detector" --emitter="./resolve.py:Emitter"

default:
	mkdir -p dst config
	swaggerknife json2swagger ${EXTRA} --name=config config.yaml --dst dst/config-spec.yaml
	swagger2go dst/config-spec.yaml --position=config --file=config.go --ref="#/definitions/config"
	goimports -w config/*.go
