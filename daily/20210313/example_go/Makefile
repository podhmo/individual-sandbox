export PORT ?= 33333
# dialect, db = parse(sqlite://:memory:)

00:
	go run $(shell echo $@*)/main.go
x00:
	http GET :$(PORT)/
t00:
	go test ./$(shell echo $(patsubst t%,%,$@)*)/

01:
	go run $(shell echo $@*)/main.go
t01:
	go test -v ./$(shell echo $(patsubst t%,%,$@)*)/

02:
	go run $(shell echo $@*)/main.go
t02:
	go test -v ./$(shell echo $(patsubst t%,%,$@)*)/

03:
	go run $(shell echo $@*)/main.go
t03:
	go test -v ./$(shell echo $(patsubst t%,%,$@)*)/
