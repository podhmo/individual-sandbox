00: PIPENAME ?= foo.pipe
00:
	rm -f $(PIPENAME)
	mkfifo $(PIPENAME)
	go run $(shell echo $@*/)main.go --name=$(PIPENAME)&
	sleep 1
	cat test.txt > $(PIPENAME)
01:
	go run $(shell echo $@*-server/)main.go&
	sleep 1
	go run $(shell echo $@*-client/)main.go
