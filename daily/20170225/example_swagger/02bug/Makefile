REPO = ${GOPATH}/src/github.com/$(shell me 2>/dev/null || echo 'foo')
PKG = ${REPO}/boo

default:
	dictknife concat swagger.yml --dst swagger.json
	swagger generate server -t .
	test -d ${REPO} || mkdir -p ${REPO}
	(test -f ${PKG} && rm ${PKG}) || echo ok
	ln -s `pwd` ${PKG} || echo ok

clean:
	rm -rf `ls restapi/* | grep -v configure_` cmd

build:
	go install -v ./...

run: default build
	x02bug-server --port 4040

setup:
	go get -v github.com/go-swagger/go-swagger/cmd/swagger
	go get -v github.com/go-openapi/runtime
	go get -v github.com/tylerb/graceful
	go get -v github.com/docker/go-units
