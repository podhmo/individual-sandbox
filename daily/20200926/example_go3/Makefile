00:
	go run $(shell echo $@*)/main.go
01:
	go run $(shell echo $@*)/main.go
02:
	go run $(shell echo $@*)/main.go

gen:
	go generate ./...
.PHONY: gen

init: setup
	entc init User
.PHONY: init

setup:
	go get -v github.com/facebook/ent/cmd/entc
.PHONY: setup
