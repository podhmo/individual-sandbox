PKG = ${GOPATH}/src/github.com/podhmo/hmm2

setup:
	go get -v github.com/golang/mock/gomock
	go get -v github.com/golang/mock/mockgen

link:
	mkdir -p `dirname ${PKG}`
	ln -s `pwd` ${PKG}

gen:
	go generate

test:
	go test -v
