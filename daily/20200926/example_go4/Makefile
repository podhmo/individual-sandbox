export SLACK_TOKEN ?= $(shell jq -r '.slack["api-token"]'  ~/.config/monogusa/config.json)

00:
	@echo $(SLACK_TOKEN)

01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go
03:
	go run $(shell echo $@*/)main.go
04:
	DEBUG=1 go run $(shell echo $@*/)main.go
05:
	go test -v ./$(shell echo $@*/)
06:
	go test -v ./$(shell echo $@*/)
06-run:
	go run ./$(shell echo $(subst -run,,$@)*/)main.go
07:
	go test -v ./$(shell echo $@*/)
