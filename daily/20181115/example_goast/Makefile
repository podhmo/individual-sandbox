default: 00 01 02
TARGET ?= ./hello/main.go

00:
	go run 00*/main.go | tee 00.txt
01:
	go run 01*/main.go | tee 01.txt
02:
	go run 02*/main.go | tee 02.txt
03:
	mkdir -p 03merge/output
	go run 03*/main.go | tee 03merge/output/main.go
	go run 03*/output/main.go
04:
	go run ./04wraprun/ ${TARGET}
	git checkout ${TARGET}

05:
	go run ./05gorun/ ${TARGET}
	git checkout ${TARGET}

06:
	$(MAKE) 04 TARGET=get
