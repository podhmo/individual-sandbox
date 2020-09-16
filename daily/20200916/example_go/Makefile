test:
	go test ./store
.PHONY: test

export PORT ?= :50052
run-server:
	go run cmd/grpc-server/main.go
run-client:
	go run cmd/grpc-client/main.go

gen: SOURCE ?= todo.proto
gen:
	protoc $(SOURCE) \
 --go_out=plugins=grpc:todorpc \
 --doc_out=html,index.html:./ \

.PHONY: gen
