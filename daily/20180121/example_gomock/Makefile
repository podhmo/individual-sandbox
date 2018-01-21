test:
	go test -v

gen: gen-gomock gen-testify-mock

gen-gomock:
	mkdir -p gengomock
	mockgen -source hello/hello.go -package gengomock -destination ./gengomock/hello.go

gen-testify-mock:
	mkdir -p genmock
	mockery -all -dir ./hello -outpkg genmock -output ./genmock/

setup:
	go get -v github.com/golang/mock/...
	go get -v github.com/vektra/mockery/...
