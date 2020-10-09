DIR = $(shell echo $@*/)

00:
	go run $(DIR)main.go | tee $(DIR)openapi.json
01:
	FILENAME=$(shell pwd)/$(DIR)main.go go run $(DIR)main.go
02:
	GODEBUG=1 go run $(DIR)main.go
03:
	go run $(DIR)main.go
04:
	go run $(DIR)main.go
05:
	go run $(DIR)main.go
