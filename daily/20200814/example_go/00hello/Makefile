test:
	go test -test.cpuprofile=x.prof -test.trace=x.trace -test.v

trace:
	go tool trace x.trace
profile:
	go tool pprof -http :33333 x.prof
