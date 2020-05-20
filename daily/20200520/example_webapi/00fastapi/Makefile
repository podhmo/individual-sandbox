export PORT ?= 44444
URL := http://localhost:${PORT}

00:
	uvicorn --port=${PORT} main:app
01:
	python show_doc.py | tee openapi.json

post-ng:
	echo '{"ttitle": "hello world", "content": "this is first article"}' | http -v --json post ${URL}/api/articles
post:
	echo '{"title": "hello world", "content": "this is first article"}' | http -v --json post ${URL}/api/articles
