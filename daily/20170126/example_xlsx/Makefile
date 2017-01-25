default:
	python gen.py 00simple/workbook.json > 00simple/main.go
	goimports -w 00simple/*.go
	(cd 00simple; go run main.go)


install:
	go get -v github.com/tealeg/xlsx
