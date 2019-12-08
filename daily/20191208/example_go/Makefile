00:
	go run $@*/main.go

01:
	ioknife too \
 --cmd "go run $$(echo $@*)/main.go 44444 http://localhost:55555" \
 --cmd "go run hello/main.go 55555" \
	# http :44444
02:
	go run $@*/main.go 44444
	# http :44444/static/go.mod
hello:
	go run hello/main.go 55555
	# http :55555/
.PHONY: hello
