SHELL := bash

00:
	go run $(shell echo $@*)/main.go
x00: tdir = $(shell echo $(patsubst x%,%,$@)*/)
x00:
	go build -gcflags="-S -N" $(tdir)main.go |& sed 's@$(shell pwd)@$.@g' | tee $(tdir)output.txt
	rm main

01:
	go run $(shell echo $@*)/main.go
x01: tdir = $(shell echo $(patsubst x%,%,$@)*/)
x01:
	go build -gcflags="-S -N" $(tdir)main.go |& sed 's@$(shell pwd)@$.@g' | tee $(tdir)output.txt
	rm main
