default:
	json2swagger --name conf --annotations annotations.yaml ./config.yaml > swagger.yaml
	python gen.py swagger.yaml --package main --ref "#/definitions/conf" > conf.go
	goimports -w conf.go
	dictknife concat config.yaml -f json | go run *.go

setup:
	pip install dictknife json2swagger
	go get -v github.com/k0kubun/pp
