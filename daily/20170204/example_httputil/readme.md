reverse proxy

```bash
$ go run 02*/main.go
GET http://qiita.com/api/v1/search?q=go HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
User-Agent: HTTPie/0.9.2
X-Forwarded-For: 127.0.0.1


HTTP/1.1 301 Moved Permanently
Content-Length: 102
Cache-Control: no-cache
Connection: keep-alive
Content-Type: text/html; charset=utf-8
Date: Fri, 03 Feb 2017 16:40:01 GMT
Location: https://qiita.com/api/v1/search?q=go
Server: nginx
Vary: Origin
X-Content-Type-Options: nosniff
X-Frame-Options: SAMEORIGIN
X-Request-Id: 71ac2bea-059d-4994-ba61-f7bb7771dc10
X-Runtime: 0.033801
X-Xss-Protection: 1; mode=block
```

client

```
$ http --follow --proxy=http://localhost:8080 GET "http://qiita.com/api/v1/search?q=go"
```
