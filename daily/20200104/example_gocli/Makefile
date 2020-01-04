00:
	go run $@*/main.go world | tee $(shell echo $@*/)main.output
	go run $@*/main.go --help 2>&1 | tee $(shell echo $@*/)help.output
01:
	go run $@*/main.go register world | tee $(shell echo $@*/)register.output
	go run $@*/main.go --help 2>&1 | tee $(shell echo $@*/)help.output
	go run $@*/main.go register --help 2>&1 | tee $(shell echo $@*/)register-help.output
