export ADDR := :33333
SHELL := bash

00:
	go run $(shell echo $@*/)cmd/foo-api/main.go
x00:
	echo '{"user": {"name": "foo"}}' | http --json POST $(ADDR)/Hello
	echo '{"user": {"name": "foo"}}' | http --json POST $(ADDR)/Hello short==true
x01:
	echo '{"v": 10}' | http --json POST $(ADDR)/IsEven
	echo '{"v": 9}' | http --json POST $(ADDR)/IsEven
	echo '{"v": "foo"}' | http --json POST $(ADDR)/IsEven
x02:
	go run $(shell echo 00*/)cmd/foo-cli/main.go AddTodo --todo @todo.json
	go run $(shell echo 00*/)cmd/foo-cli/main.go AddTodo --todo @<(echo '{"title": "xxx"}' )

x03:
	cat todo.json | http --json POST $(ADDR)/AddTodo
	cat todo.json | http --json POST $(ADDR)/AddTodo
	http --json POST $(ADDR)/ListTodo
x04:
	echo '{"name": "foo"}' | http --json POST $(ADDR)/Greet

01:
	go run $(shell echo $@*/)main.go
02:
	go run $(shell echo $@*/)main.go
