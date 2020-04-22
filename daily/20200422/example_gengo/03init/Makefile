default: clean definitions.py
	python definitions.py generate
	go run cmd/hello/main.go -h

definitions.py:
	gogen init

clean:
	rm -rf cmd definitions.py
.PHONY: clean
