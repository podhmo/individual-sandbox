export ADDR ?= :33333

00:
	go run $(shell echo $@*/)main.go
x00:
	echo '{"name": "foo"}' | http --json POST $(ADDR)/
01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go
03:
	go run $(shell echo $@*/)main.go
04:
	go run $(shell echo $@*/)main.go
