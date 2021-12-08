00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go

# with gen

02:
	go run $(shell echo $@*/)main.go
03:
	go run $(shell echo $@*/)main.go

gen:
	go install github.com/kyleconroy/sqlc/cmd/sqlc@latest
	sqlc generate