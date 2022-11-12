00:
	go run ./$(shell echo $@*/)
01:
	go run ./$(shell echo $@*/)	
02:
	go run ./$(shell echo $@*/)	
03:
	go run ./$(shell echo $@*/)	-debug ./$(shell echo $@*/)internal/foo.go
04:
	( cd ./$(shell echo $@*/); go test -bench . )

04-pprof:
	( cd ./$(shell echo 04*/); go test -bench . -benchmem -cpuprofile cpu.out )
# go tool pprof -http=localhost:33333 cpu.out