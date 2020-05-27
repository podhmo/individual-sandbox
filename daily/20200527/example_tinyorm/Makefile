00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go
	diff -u $(shell echo 00*/)main.go $(shell echo $@*/)main.go > $(shell echo $@*/)a.diff || exit 0
