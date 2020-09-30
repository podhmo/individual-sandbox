export GOOGLE_CREDENTIALS ?= $(shell jq -r '.misc["google-tasks-api"]' ~/.config/monogusa/config.json )

00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go
03:
	go run $(shell echo $@*/)main.go

setup:
	go get -u -v google.golang.org/api/tasks/v1
	go get -u -v golang.org/x/oauth2/google
