default: 00 01

00:
	rm -f 00.output
	python 00*.py >> 00.output
	TMPDIR=. python 00*.py >> 00.output
	TEMP=. python 00*.py >> 00.output

01:
	rm -f 01.output
	go run 01*.go >> 01.output
	TMPDIR=. go run 01*.go >> 01.output
