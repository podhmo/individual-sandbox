test:
	go test ./store
.PHONY: test

export PORT ?= :50052
run-server:
	go run cmd/grpc-server/main.go
run-client:
	go run cmd/grpc-client/main.go

web-server:
	go run cmd/web-server/main.go
web-client:
	http $(PORT)/api/todos
	echo '{"title": "Go to bed"}' | jq . |  http --json POST $(PORT)/api/todos
	http $(PORT)/api/todos

gen: SOURCE ?= todo.proto
gen:
	protoc $(SOURCE) \
 --go_out=plugins=grpc:todorpc \
 --doc_out=html,index.html:./ \

.PHONY: gen
