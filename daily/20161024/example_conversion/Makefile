WHO ?= podhmo
PKG = ${GOPATH}/src/github.com/${WHO}/hmm

default: json convert

install:
	go get -u -v github.com/podhmo/go-structjson/cmd/go-structjson

sync:
	cp ${GOPATH}/src/github.com/podhmo/sandbox/convert/convert.py .
	cp -r ${GOPATH}/src/github.com/podhmo/sandbox/convert/tests/* tests/

json:
	go-structjson --target model | jq . -S > output/model.json
	go-structjson --target def | jq . -S > output/def.json

masking:
	gsed -i "s@`pwd`@.@g" output/*.json
	gsed -i "s@`echo $$GOPATH`@GOPATH@g" output/*.json

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

test:
	PYTHONPATH=. python -m unittest discover tests


.PHONY: json convert default setup clean run
