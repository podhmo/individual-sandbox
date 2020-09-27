export ADDR := :44444

# http :44444/api
00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go
03:
	go run $(shell echo $@*/)main.go
04:
	go run $(shell echo $@*/)main.go
05:
	go run $(shell echo $@*/)main.go
06:
	go test -v ./$(shell echo $@*/)

# curl -v http://localhost:44444/api/xxx
# curl -v http://localhost:44444/api/yyy
# curl -v http://localhost:44444/api/xxx2?pretty=1
# curl -v http://localhost:44444/api/yyy2?pretty=1
07:
	go run $(shell echo $@*/)main.go

# echo '{"title": "hello"}' | http --json POST :44444/api/board/xxx/todo
# echo '{"title": "hello"}' | http --json POST :44444/api/board/xxx/todo2
08:
	go run $(shell echo $@*/)main.go
09:
	go run $(shell echo $@*/)main.go
10:
	cd ./$(shell echo $@*/) && go test -v -test.testlogfile=log.txt
11:
	cd ./$(shell echo $@*/) && go test -v -test.testlogfile=log.txt
