## log

```console
$ make
uvicorn --port=44444 main:app
INFO:     Started server process [15699]
INFO:     Uvicorn running on http://127.0.0.1:44444 (Press CTRL+C to quit)
INFO:     Waiting for application startup.
INFO:     Application startup complete.
INFO:     127.0.0.1:52724 - "POST /api/articles HTTP/1.1" 200 OK
INFO:     127.0.0.1:52726 - "POST /api/articles HTTP/1.1" 422 Unprocessable Entity
```

## post

```console
$ echo '{"title": "hello world", "content": "this is first article"}' | http -v --json post http://localhost:44444/api/articles
POST /api/articles HTTP/1.1
Accept: application/json, */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 61
Content-Type: application/json
Host: localhost:44444
User-Agent: HTTPie/1.0.3

{
    "content": "this is first article",
    "title": "hello world"
}

HTTP/1.1 200 OK
content-length: 57
content-type: application/json
date: Wed, 20 May 2020 08:57:53 GMT
server: uvicorn

{
    "content": "this is first article",
    "title": "hello world"
}

```


## post ng

```console
$ echo '{"ttitle": "hello world", "content": "this is first article"}' | http -v --json post http://localhost:44444/api/articles
POST /api/articles HTTP/1.1
Accept: application/json, */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 62
Content-Type: application/json
Host: localhost:44444
User-Agent: HTTPie/1.0.3

{
    "content": "this is first article",
    "ttitle": "hello world"
}

HTTP/1.1 422 Unprocessable Entity
content-length: 96
content-type: application/json
date: Wed, 20 May 2020 08:59:08 GMT
server: uvicorn

{
    "detail": [
        {
            "loc": [
                "body",
                "item",
                "title"
            ],
            "msg": "field required",
            "type": "value_error.missing"
        }
    ]
}

```
