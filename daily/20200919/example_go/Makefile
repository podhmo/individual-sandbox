SHELL := $(shell which bash)

00:
	go run $(shell echo $@*/)main$@.go |& sed 's@$(shell pwd)/@./@g' | tee $@.output
01:
	go run $(shell echo $@*/)main$@.go |& sed 's@$(shell pwd)/@./@g' | tee $@.output
02:
	go run $(shell echo $@*/)main$@.go |& sed 's@$(shell pwd)/@./@g' | tee $@.output
	diff -u 00*/main*.go 02*/main*.go > 0002.diff
03:
	go run $(shell echo $@*/)main$@.go |& sed 's@$(shell pwd)/@./@g' | tee $@.output
