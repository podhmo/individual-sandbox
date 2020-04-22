default:
	python definitions.py generate
	go run cmd/hello/main.go --name world

definitions.py:
	gogen init

clean:
	rm -rf cmd definitions.py
.PHONY: clean
