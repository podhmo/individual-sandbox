dev:
	go run . -p 8080

front:
	cd frontend && npm run dev

release: dist/sample

dist/sample:
	mkdir -p dist
	go generate -tags release ./...
	go build -tags release -o $@ .