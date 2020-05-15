SHELL := $(shell which bash)

gen:
	python definitions.py generate

# ng
00:
	go run cmd/load/main.go --filename <(echo '{}')
# ok
01:
	go run cmd/load/main.go --filename <(echo '{"title": "hello world", "content": "Hello worlld, this is my first article. ...", "comments": []}')
# ng
02:
	go run cmd/load/main.go --filename <(echo '{"comments": [{}], "title": "hello world", "content": "Hello worlld, this is my first article. ..."}')
