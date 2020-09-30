export ADDR ?= :44444
# http :44444/hello
# http :44444/hello Authorization:"Bearer ak:user:0"
# http :44444/hello Authorization:"Bearer ak:user:1"

00:
	cd $(shell echo $@*/) && go test -v
00-run:
	go run $(shell echo $(subst $@,-run,)*/)main.go
