export ADDR ?= :44444

# open http://localhost:44444/graphql
00:
	cd $(shell echo $@*/) && go test -v
00-run:
	go run $(shell echo $(subst $@,-run,)*/)main.go
