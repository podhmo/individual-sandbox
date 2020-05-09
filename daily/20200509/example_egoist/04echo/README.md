## log

```console
$ make
go run main.go

   ____    __
  / __/___/ /  ___
 / _// __/ _ \/ _ \
/___/\__/_//_/\___/ v4.1.16
High performance, minimalist Go web framework
https://echo.labstack.com
____________________________________O/_______
                                    O\
⇨ http server started on [::]:44444
{"time":"2020-05-09T17:19:55.007957+09:00","id":"","remote_ip":"127.0.0.1","host":"localhost:44444","method":"POST","uri":"/api/articles","user_agent":"HTTPie/1.0.3","status":200,"error":"","latency":280607,"latency_human":"280.607µs","bytes_in":61,"bytes_out":58}
{"time":"2020-05-09T17:20:16.465856+09:00","id":"","remote_ip":"127.0.0.1","host":"localhost:44444","method":"POST","uri":"/api/articles","user_agent":"HTTPie/1.0.3","status":400,"error":"code=400, message=Key: 'Article.Title' Error:Field validation for 'Title' failed on the 'required' tag","latency":184805,"latency_human":"184.805µs","bytes_in":62,"bytes_out":99}
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
Content-Type: application/json; charset=UTF-8
Date: Sat, 09 May 2020 08:19:55 GMT
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
Content-Type: application/json; charset=UTF-8
Date: Sat, 09 May 2020 08:20:16 GMT
Content-Length: 99

{"message":"Key: 'Article.Title' Error:Field validation for 'Title' failed on the 'required' tag"}
```
