00:
	go run $(shell echo $@*/)main.go
01:
	go test ./$(shell echo $@*/)
	rm -rf ./01jsend/{x,y,z}
	go clean ./01jsend && CAPTURE_WRITEFILE=x go test -count=1 ./01jsend/
	go clean ./01jsend && CAPTURE_LAYOUT=json CAPTURE_WRITEFILE=y go test -count=1 ./01jsend/
	go clean ./01jsend && CAPTURE_LAYOUT=openapi CAPTURE_WRITEFILE=z go test -count=1 ./01jsend/
02:
	go run $(shell echo $@*/)main.go
03:
	go run $(shell echo $@*/)main.go

06:
	go run $(shell echo $@*/)main.go | tee $(shell echo $@*/)x.har

shapes:
	dictknife shape --with-type -i json 02*/trace.json > shapes/tenuki.trace
	dictknife shape --with-type -i json 04*/shrink.har > shapes/har.trace
.PHONY: shapes
