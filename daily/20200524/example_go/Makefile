00:
	go run $(shell echo $@*/)main$@.go
01:
	go run $(shell echo $@*/)main$@.go
02:
	go run $(shell echo $@*/)main$@.go
03:
	go run $(shell echo $@*/)main$@.go
04:
	make gen -C $(shell echo $@*/)
	go run $(shell echo $@*/)main$@.go
04-smd:
	http --json GET :9999/ smd==""
04-client:
	echo '{"jsonrpc": "2.0", "method": "multiply", "params": {"a": 42, "b": 23}, "id": 1}' | http --json POST :9999/

05:
	go run $(shell echo $@*/)server/main$@.go
