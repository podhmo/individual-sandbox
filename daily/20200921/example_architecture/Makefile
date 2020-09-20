00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go

03: STORE = $(shell echo $@*/)store.json
03:
	go run $(shell echo $@*/)cmd/cli/main.go \
 -store $(STORE) \
 list
	go run $(shell echo $@*/)cmd/cli/main.go \
 -store $(STORE) \
 -save \
 add hello
	go run $(shell echo $@*/)cmd/cli/main.go \
 -store $(STORE) \
 list
03-repl:
	go run $(shell echo 03*)/cmd/repl/main.go
03-web:
	go run $(shell echo 03*)/cmd/web/main.go
