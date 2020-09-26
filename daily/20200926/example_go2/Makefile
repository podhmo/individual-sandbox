00: resource.db
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*)/main.go
02:
	go run $(shell echo $@*)/main.go
03: resource.db
	go run $(shell echo $@*)/main.go
04: resource.db
	go run $(shell echo $@*)/main.go
05: resource.db
	go run $(shell echo $@*)/main.go

resource.db:
	cat insert.sql | sqlite3 resource.db
clean:
	rm -f resource.db
.PHONY: clean
