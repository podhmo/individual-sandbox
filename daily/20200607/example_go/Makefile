00:
	go run $(shell echo $@*/)main.go
01:
	go run $(shell echo $@*/)main.go
01-query:
	echo '{"query": "$(shell cat starwars.ql)"}' | http --json POST :8080/graphql
