server:
	go run main.go
get:
	http :3333/
post:
	echo '{"message": "hello world"}' | http --json POST :3333/
get-hello:
	http :3333/hello/world
get-404:
	http :3333/404
