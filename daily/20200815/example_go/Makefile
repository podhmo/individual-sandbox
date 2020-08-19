00:
	go run $(shell echo $@*)/main.go Makefile
	cat Makefile | go run $(shell echo $@*)/main.go
01:
	go run $(shell echo $@*)/main.go
02:
	go run $(shell echo $@*)/main.go Makefile Makefile
	cat Makefile | go run $(shell echo $@*)/main.go
03:
	go run $(shell echo $@*)/main.go Makefile Makefile
	cat Makefile | go run $(shell echo $@*)/main.go

TARGET := ~/Downloads/Docker.dmg
bench: BIN=/tmp
bench:
	go build -o $(BIN)/02 ./$(shell echo 02*)
	go build -o $(BIN)/03 ./$(shell echo 03*)
	go build -o $(BIN)/04 ./$(shell echo 04*)
	@echo "----------------------------------------"
	time cat $(TARGET) > /dev/null
	time $(BIN)/02 $(TARGET) > /dev/null
	time $(BIN)/03 $(TARGET) > /dev/null
	time $(BIN)/04 $(TARGET) > /dev/null
	@echo "----------------------------------------"
	time cat $(TARGET) | cat $(TARGET) > /dev/null
	time cat $(TARGET) | $(BIN)/02 $(TARGET) > /dev/null
	time cat $(TARGET) | $(BIN)/03 $(TARGET) > /dev/null
	time cat $(TARGET) | $(BIN)/04 $(TARGET) > /dev/null
