00:
	mkdir -p 00
	python 00gen.py 100 > 00/main.go
	goimports -w 00/main.go
	go run 00/main.go | tee 00.output
.PHONY: 00
01:
	mkdir -p 01
	python 01gen.py 100 > 01/main.go
	goimports -w 01/main.go
	go run 01/main.go | tee 01.output
.PHONY: 01
