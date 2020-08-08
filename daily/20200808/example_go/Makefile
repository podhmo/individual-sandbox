# export URL ?= https://httpbin.org/ip
# export URL ?= https://httpbin.org/status/418
export URL ?= https://httpbin.org/headers

00:
	GOBIN=/tmp go install $(shell echo $@*/)main$@.go && /tmp/main$@
01:
	GOBIN=/tmp go install $(shell echo $@*/)main$@.go && /tmp/main$@
02:
	go run $(shell echo $@*/)main$@.go
03:
	go run $(shell echo $@*/)main$@.go
04:
	go run $(shell echo $@*/)main$@.go
05:
	go run $(shell echo $@*/)main$@.go

gen: PWD = $(shell pwd)
gen: SRC = /local/openapi.yaml
gen: DST = /local/generated
gen: PKG = generated
gen:
	rm -rf generated
	docker run --rm -v "$(PWD):/local" openapitools/openapi-generator-cli generate -i $(SRC) -g go -o $(DST) --package-name $(PKG)
	rm generated/go{.sum,.mod}
gen2:
	rm -rf generated2
	mkdir generated2
	oapi-codegen -package generated2 -generate "client,types" openapi.yaml > generated2/client.go

setup:
	# hmm. for AddPassthrough()
	go get github.com/dnaeon/go-vcr@master
