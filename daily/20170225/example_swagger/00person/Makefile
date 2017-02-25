REPO = ${GOPATH}/src/github.com/$(shell me 2>/dev/null || echo 'foo')
PKG = ${REPO}/foo

default:
	dictknife concat swagger.yaml --dst swagger.json
	swagger generate model -t .
	test -d ${REPO} || mkdir -p ${REPO}
	(test -f ${PKG} && rm ${PKG}) || echo ok
	ln -s `pwd` ${PKG}

complex:
	dictknife concat complex.yaml --dst complex.json
	swagger generate model -t . -f complex.json -m complexmodels

setup:
	go get -v github.com/go-swagger/go-swagger/cmd/swagger
	go get -v github.com/go-openapi/runtime
