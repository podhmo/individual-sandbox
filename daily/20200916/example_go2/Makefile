test:
	go test ./store
.PHONY: test

export PORT ?= :50052
web-server:
	go run cmd/web-server/main.go
web-client:
#	$(MAKE) web-client--new-item
	$(MAKE) web-client--404

web-client--new-item:
	http $(PORT)/api/todos
	echo '{"title": "Go to bed"}' | jq . |  http --json POST $(PORT)/api/todos
	http $(PORT)/api/todos

web-client--404:
	http $(PORT)/api/missing # 404
