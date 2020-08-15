OPTS ?= -bench=. -benchmem -test.cpuprofile=prof.out -count=2 -benchtime=	1s

00:
	cd $(shell echo $@*/)&& GOBIN=/tmp go install -v . && /tmp/$(shell echo $@*)
	cd $(shell echo $@*/)&& go test -v $(OPTS)
01:
	cd $(shell echo $@*/)&& GOBIN=/tmp go install -v . && /tmp/$(shell echo $@*)
	cd $(shell echo $@*/)&& go test -v $(OPTS)
02:
	cd $(shell echo $@*/)&& GOBIN=/tmp go install -v . && /tmp/$(shell echo $@*)
	cd $(shell echo $@*/)&& go test -v $(OPTS)
03:
	cd $(shell echo $@*/)&& GOBIN=/tmp go install -v . && /tmp/$(shell echo $@*)
	# cd $(shell echo $@*/)&& go test -v $(OPTS)
04:
	cd $(shell echo $@*/)&& GOBIN=/tmp go install -v . && /tmp/$(shell echo $@*)
10:
	go run $(shell echo $@*/)main.go

# go tool pprof -http=:33333 xxx/prof.out
