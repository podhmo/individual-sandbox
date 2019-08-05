tutorial:
	cd $@&& wire && go run $$(echo *.go | tr " " "\n" | grep -v wire.go)
.PHONY: tutorial

00:
	go run $@*/main.go
01:
	cd $@*&& wire && go run $$(echo *.go | tr " " "\n" | grep -v wire.go)
02:
	cd $@*&& wire && go run $$(echo *.go | tr " " "\n" | grep -v wire.go)
03:
	cd $@*&& wire && go run $$(echo *.go | tr " " "\n" | grep -v wire.go)

setup:
	go get -v github.com/google/wire/cmd/wire
