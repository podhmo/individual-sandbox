# export GOPACKAGESDEBUG := 1

TARGET ?= m/internal/s
# TARGET ?= go/doc

00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go ../readme.md
03:
	go run $(shell echo $@*/)main.go $(TARGET)
04:
	go run $(shell echo $@*/)main.go $(TARGET)
05:
	go run $(shell echo $@*/)main.go $(TARGET)
06:
	go run $(shell echo $@*/)main.go $(TARGET)
07:
	go run $(shell echo $@*/)main.go $(TARGET)
08: TARGET := m/internal/g
08:
	go run $(shell echo $@*/)main.go $(TARGET)

astdump:
	go run ./internal/astdump/main.go $(TARGET)
.PHONY: astdump
builtindump:
	go run ./internal/builtindump/main.go builtin
.PHONY: builtindump
