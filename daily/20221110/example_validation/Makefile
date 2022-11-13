00:
	go run ./$(shell echo $@*/)
01:
	go run ./$(shell echo $@*/)	
02:
	go run ./$(shell echo $@*/)	
03:
	go run ./$(shell echo $@*/)	-debug ./$(shell echo $@*/)internal/foo.go
04:
	go test -benchmem -bench . ./$(shell echo $@*/)

04-pprof:
	go test -bench .VM -cpuprofile cpu.out -outputdir . ./$(shell echo 04*/)
#	go tool pprof -http=localhost:33333 cpu.out