WHO ?= $(shell whoami)
PKG = ${GOPATH}/src/github.com/${WHO}/models

setup:
	mkdir -p `dirname ${PKG}`
	ln -s `pwd`/models ${PKG}
