## log

```console
$ make
go run main.go
[GIN-debug] [WARNING] Creating an Engine instance with the Logger and Recovery middleware already attached.

[GIN-debug] [WARNING] Running in "debug" mode. Switch to "release" mode in production.
 - using env:	export GIN_MODE=release
 - using code:	gin.SetMode(gin.ReleaseMode)

[GIN-debug] POST   /api/articles             --> main.run.func1 (3 handlers)
[GIN-debug] Environment variable PORT="44444"
[GIN-debug] Listening and serving HTTP on :44444
[GIN] 2020/05/09 - 16:46:55 | 200 |     362.189µs |       127.0.0.1 | POST     "/api/articles"
[GIN] 2020/05/09 - 16:47:22 | 400 |     178.076µs |       127.0.0.1 | POST     "/api/articles"
  C-c C-csignal: interrupt
```

## post

```
echo '{"title": "hello world", "content": "this is first article"}' | http -v --json post http://localhost:44444/api/articles
POST /api/articles HTTP/1.1
User-Agent: HTTPie/1.0.3
Accept-Encoding: gzip, deflate
Accept: application/json, */*
Connection: keep-alive
Content-Type: application/json
Content-Length: 61
Host: localhost:44444

{"title": "hello world", "content": "this is first article"}


HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8
Date: Sat, 09 May 2020 07:46:55 GMT
Content-Length: 57

{"title":"hello world","content":"this is first article"}
```


## post ng

```console
echo '{"ttitle": "hello world", "content": "this is first article"}' | http -v --json post http://localhost:44444/api/articles
POST /api/articles HTTP/1.1
User-Agent: HTTPie/1.0.3
Accept-Encoding: gzip, deflate
Accept: application/json, */*
Connection: keep-alive
Content-Type: application/json
Content-Length: 62
Host: localhost:44444

{"ttitle": "hello world", "content": "this is first article"}


HTTP/1.1 400 Bad Request
Date: Sat, 09 May 2020 07:47:22 GMT
Content-Length: 88
Content-Type: text/plain; charset=utf-8

{"error":"Key: 'Title' Error:Field validation for 'Title' failed on the 'required' tag"}
```
