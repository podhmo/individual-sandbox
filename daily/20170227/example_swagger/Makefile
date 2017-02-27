REPO = ${GOPATH}/src/github.com/$(shell me 2>/dev/null || echo 'foo')
PKG = ${REPO}/bee

default:
	swagger generate model -f swagger.yaml
	test -d ${REPO} || mkdir -p ${REPO}
	(test -f ${PKG} && rm ${PKG}) || echo ok
	ln -s `pwd` ${PKG} || echo ok
