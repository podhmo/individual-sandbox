hello: server.pid
	# curl http://localhost:8080/world
	go run client.go world

start: server.pid

server.pid:
	{ go run server.go & echo $$! > $@; }

clean: server.pid
	kill `cat $<` && rm $<

.PHONY: hello start clean
