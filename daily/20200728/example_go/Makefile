# http :4444/hello
00:
	ADDR=":4444" go run $(shell echo $@*/)main.go
00-test:
	go test ./$(shell echo 00*/)

# http :4444/api/users/xxxx
01:
	ADDR=":4444" go run $(shell echo $@*/)main.go
# 01-test:
# 	go test ./$(shell echo 01*/)
