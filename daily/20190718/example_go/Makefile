00:
	$(MAKE) gen | grep -v make | tee 00.output

gen:
	go run 00*/main.go person.json
	go run 00*/main.go signal.json
