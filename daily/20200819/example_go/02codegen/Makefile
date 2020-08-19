test: gen
	go test -v ./send

gen:
	VERBOSE=1 go run cmd/gen/main.go m/send send Notificator Notify > ./send/render_test.go
	goimports -w ./send/render_test.go
