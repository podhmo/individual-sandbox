SHELL := $(shell which bash)

default:
	$(MAKE) 00 |& tee $(shell echo 00*)/main.output

00:
	go run $(shell echo $@*/)main.go -int64 10
	go run $(shell echo $@*/)main.go --int64 10
	go run $(shell echo $@*/)main.go -int64=10
	go run $(shell echo $@*/)main.go --int64=10
	go run $(shell echo $@*/)main.go --help
	go run $(shell echo $@*/)main.go -h
01:
	go run $(shell echo $@*/)main.go -h
02:
	go run $(shell echo $@*/)main.go
	go run $(shell echo $@*/)main.go --int64=64
	go run $(shell echo $@*/)main.go --int64=foo
03:
	go run $(shell echo $@*/)main.go foo
	go run $(shell echo $@*/)main.go bar
	go run $(shell echo $@*/)main.go
06:
	go run $(shell echo $@*/)main.go
	go run $(shell echo $@*/)main.go xxx
	go run $(shell echo $@*/)main.go foo
