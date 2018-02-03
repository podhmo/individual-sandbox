gen:
	ffjson ${GOPATH}/src/github.com/podhmo/sandbox/model/person.go

run:
	go run main.go

bench:
	go test -bench=. -benchmem

benchprof:
	go test -bench=. -benchmem -cpuprofile=/tmp/bench.prof
	go tool pprof /tmp/bench.prof
