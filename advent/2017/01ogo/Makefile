build:
	(cd go/src/; GOROOT_BOOTSTRAP=`go env | grep GOROOT | cut -d = -f 2` bash -x make.bash --no-clean)

run:
	go/bin/go run 01*/main.go
