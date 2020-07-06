default: 00 01 02 03 04 05

00:
	go run structscan/main.go m/$(shell echo $@*) | tee output$@.json
01:
	go run structscan/main.go m/$(shell echo $@*) | tee output$@.json
02:
	go run structscan/main.go m/$(shell echo $@*):Person | tee output$@.json
03:
	go run structscan/main.go m/$(shell echo $@*) | tee output$@.json
04:
	go run structscan/main.go m/$(shell echo $@*) | tee output$@.json
05:
	go run structscan/main.go m/$(shell echo $@*) | tee output$@.json
