SHELL := bash

00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go
03:
# can't find import
# 	go tool compile -S -N ./$(shell echo $@*/)*.go
	go build -gcflags="-S -N" ./$(shell echo $@*/)*.go |& tee output03
