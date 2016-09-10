test:
	go test

hello: server.pid
	# curl http://localhost:8080/world
	go run cmd/client/main.go world

start: server.pid

server.pid:
	{ go run cmd/server/main.go & echo $$! > $@; }

clean: server.pid
	kill `cat $<` && rm $<

.PHONY: hello start clean
