# server :8080
00:
	go run $(shell echo $@*/)main.go
01:
	echo '{"jsonrpc": "2.0", "method": "Main.Echo", "params": {"name": "John Doe"}, "id": "1"}' | http --json POST :8080/jrpc
02:
	go run $(shell echo $@*/)main.go
