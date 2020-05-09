export PORT ?= 44444
URL := http://localhost:${PORT}

00:
	go run $(shell echo $@*/)main.go --title="hello world"
01:
	go run $(shell echo $@*/)main.go --filename=$(shell echo $@*/)input.json
	go run $(shell echo $@*/)main.go --filename=$(shell echo $@*/)input2.json || exit 0
02:
	$(MAKE) -C $(shell echo $@*/)

post-ng:
	echo '{"ttitle": "hello world", "content": "this is first article"}' | http -v --json post ${URL}/api/articles
post:
	echo '{"title": "hello world", "content": "this is first article"}' | http -v --json post ${URL}/api/articles

gen:
	python definitions.py generate
