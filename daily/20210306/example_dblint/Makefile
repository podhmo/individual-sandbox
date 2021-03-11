export DB := data.db

data.db:
	cat insert.sql | sqlite3 data.db

00:
	go run $(shell echo $@*)/main.go | tee $(shell echo $@*)/output.log
01:
	go run $(shell echo $@*)/main.go
02:
	go run $(shell echo $@*)/main.go | tee $(shell echo $@*)/output.log
03:
	go run $(shell echo $@*)/main.go | tee $(shell echo $@*)/output.log
04:
	go run $(shell echo $@*)/main.go | tee $(shell echo $@*)/output.log
05:
	go run $(shell echo $@*)/main.go | tee $(shell echo $@*)/output.log
06:
	go run $(shell echo $@*)/main.go

