gen:
	swagger generate server -A todo-list -f ./swagger.yml
run:
	go run cmd/todo-list-server/main.go --port 4444
client:
	http :4444/
setup:
	go get -v github.com/go-openapi/loads
	go get -v github.com/go-openapi/runtime
	go get -v github.com/docker/go-units
	go get -v github.com/go-openapi/validate
	go get -v github.com/jessevdk/go-flags
