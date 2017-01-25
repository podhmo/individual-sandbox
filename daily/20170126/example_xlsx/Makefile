default: zero one

zero:
	python gen.py 00simple/workbook.json > 00simple/main.go
	goimports -w 00simple/*.go
	(cd 00simple; go run main.go)

one:
	python 01kuku/gen.py > 01kuku/kuku.json
	python gen.py 01kuku/kuku.json > 01kuku/main.go
	goimports -w 01kuku/*.go
	(cd 01kuku; go run main.go)

install:
	go get -v github.com/tealeg/xlsx
