00:
	(python 00*.py& echo $$! > server.pid; sleep 1)
	http :8080/openapi.json | jq . | tee 00openapi.json
	kill -TERM $$(cat server.pid)

01:
	python 01*.py | tee 01openapi.yaml
