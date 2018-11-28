default: 00 01 02

00:
	go run 00*/main.go | tee 00.txt
01:
	go run 01*/main.go | tee 01.txt
02:
	go run 02*/main.go | tee 02.txt
03:
	mkdir -p 03merge/output
	go run 03*/main.go | tee 03merge/output/main.go
	go run 03*/output/main.go
04:
	mkdir -p ${GOPATH}/src/github.com/podhmo
	rm -f ${GOPATH}/src/github.com/podhmo/wraprun
	ln -s $(shell pwd)/04wraprun ${GOPATH}/src/github.com/podhmo/wraprun
	(go install ${GOPATH}/src/github.com/podhmo/wraprun) && wraprun ./hello/main.go
	rm `which wraprun`
