default: 00 01 02 03 04 05
dst:
	mkdir -p dst

00: dst
	go run 00*/main.go |& tee dst/00.output
01: dst
	go run 01*/main.go |& tee dst/01.output
02: dst
	go run 02*/main.go |& tee dst/02.output
03: dst
	go run 03*/main.go |& tee dst/03.output
04: dst
	go run 04*/main.go |& tee dst/04.output
05: dst
	go run 05*/main.go |& tee dst/05.output
