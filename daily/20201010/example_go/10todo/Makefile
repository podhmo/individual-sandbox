export ADDR ?= :8888

run:
	go run main.go

openapi.json: main.go
	GENDOC=1 go run main.go | tee $@
