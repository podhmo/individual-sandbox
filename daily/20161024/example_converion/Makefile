WHO ?= podhmo
PKG = ${GOPATH}/src/github.com/${WHO}/hmm

default: json convert

install:
	go get -u -v github.com/podhmo/go-structjson/cmd/go-structjson

json:
	go-structjson --target model/model.go | jq . -S > output/model.json
	go-structjson --target def/def.go | jq . -S > output/def.json

convert:
	python convert.py --src output/model.json --dst output/def.json > convert/convert.go
	gofmt -w convert/convert.go

run:
	go run main.go

clean:
	rm -rf output
	rm -rf convert
	rm -f ${PKG}

setup:
	mkdir -p `dirname ${PKG}`
	ln -s `pwd` ${PKG}


.PHONY: json convert default setup clean run
