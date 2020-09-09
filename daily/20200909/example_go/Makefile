SHELL := $(shell which bash)

00:
	go run $(shell echo $@*/)main.go |& sed 's@$(shell pwd)@CWD@g' | tee $@.output
01:
	go run $(shell echo $@*/)main.go |& sed 's@$(shell pwd)@CWD@g' | tee $@.output
02:
	go run $(shell echo $@*/)main.go |& sed 's@$(shell pwd)@CWD@g' | tee $@.output
03:
	go run $(shell echo $@*/)main.go |& sed 's@$(shell pwd)@CWD@g' | tee $@.output
04:
	go run $(shell echo $@*/)main.go |& sed 's@$(shell pwd)@CWD@g' | tee $@.output
05:
	go run $(shell echo $@*/)main.go |& sed 's@$(shell pwd)@CWD@g' | tee $@.output

iface:
	$(MAKE) 04 05

diff:
	diff -u 00*/main.go 01*/main.go > 01.diff || :
	diff -u 00*/main.go 02*/main.go > 02.diff || :
	diff -u 00*/main.go 03*/main.go > 03.diff || :
	diff -u 00*/main.go 04*/main.go > 04.diff || :
	diff -u 04*/main.go 05*/main.go > 05.diff || :

vet:
	go vet ./00*/ || :
	go vet ./01*/ || :
	go vet ./02*/ || :
	go vet ./03*/ || :
	go vet ./04*/ || :
	go vet ./05*/ || :
