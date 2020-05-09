## log

```console
$ make
go run main.go
2020/05/09 16:59:35 [MacBook-Air.local/9xs4IblcpV-000001] "POST http://localhost:44444/api/articles HTTP/1.1" from 127.0.0.1:52435 - 200 58B in 346.149µs
2020/05/09 17:00:05 [MacBook-Air.local/9xs4IblcpV-000002] "POST http://localhost:44444/api/articles HTTP/1.1" from 127.0.0.1:52437 - 400 73B in 300.333µs
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
Date: Sat, 09 May 2020 07:59:35 GMT
Content-Length: 58

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
Content-Type: application/json; charset=utf-8
Date: Sat, 09 May 2020 08:00:05 GMT
Content-Length: 73

{"summary":"title, required","messages":{"title":[{"text":"required"}]}}
```
