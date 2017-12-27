#[python][reqtrace][httpie] reqtraceにrequestを良い感じに返す機能を追加した(httpbin的なもの)

[https://github.com/podhmo/reqtrace:embed:cite]

reqtraceにrequestを良い感じに返す機能を追加した。https://httpbin.org/ のようなもの。
直接何か色々触りたいときとかに欲しくなったりするので。

## server

44444で立ち上げる。portを指定しなかった場合にはテキトウに空いているportを探す。

```console
$ python -m reqtrace.commands.echo -h
usage: echo.py [-h] [--port PORT] [--logging {CRITICAL,FATAL,ERROR,WARN,WARNING,INFO,DEBUG,NOTSET}] [--callback CALLBACK]
               [--pdb]

optional arguments:
  -h, --help            show this help message and exit
  --port PORT
  --logging {CRITICAL,FATAL,ERROR,WARN,WARNING,INFO,DEBUG,NOTSET}
  --callback CALLBACK
  --pdb

$ python -m reqtrace.commands.echo --port 44444
127.0.0.1 - - [27/Dec/2017 23:24:39] "GET /hello HTTP/1.1" 200 283
127.0.0.1 - - [27/Dec/2017 23:24:52] "GET /hello?name=foo&age=20&skills=x&skills=y&skills=z HTTP/1.1" 200 368
127.0.0.1 - - [27/Dec/2017 23:27:11] "GET /auth HTTP/1.1" 200 324
127.0.0.1 - - [27/Dec/2017 23:30:20] "POST /people HTTP/1.1" 200 350
127.0.0.1 - - [27/Dec/2017 23:31:30] "POST /people HTTP/1.1" 200 348
```

## clientからテキトウにrequestしてみる。

テキトウにhttpieなどでrequestしてみる。

### get /

```console
$ http --verbose :44444/hello
HTTP/1.0 200 OK
Content-Length: 283
Content-Type: application/json; charset=utf-8
Date: Wed, 27 Dec 2017 14:23:46 GMT
Server: WSGIServer/0.2 CPython/3.6.2

{
    "accept": "*/*",
    "accept_encoding": "gzip, deflate",
    "connection": "keep-alive",
    "content_length": "",
    "content_type": "text/plain",
    "host": "localhost:44444",
    "method": "GET",
    "path": "/hello",
    "querystring": "",
    "url": "http://localhost:44444/hello",
    "user_agent": "HTTPie/0.9.9"
}
```

### query stringを付与してみる

```console
$ http :44444/hello name==foo age==20 skills==x skills==y skills==z
HTTP/1.0 200 OK
Content-Length: 368
Content-Type: application/json; charset=utf-8
Date: Wed, 27 Dec 2017 14:24:52 GMT
Server: WSGIServer/0.2 CPython/3.6.2

{
    "accept": "*/*",
    "accept_encoding": "gzip, deflate",
    "connection": "keep-alive",
    "content_length": "",
    "content_type": "text/plain",
    "host": "localhost:44444",
    "method": "GET",
    "path": "/hello",
    "querystring": "name=foo&age=20&skills=x&skills=y&skills=z",
    "url": "http://localhost:44444/hello?name=foo&age=20&skills=x&skills=y&skills=z",
    "user_agent": "HTTPie/0.9.9"
}
```

### basic認証のrequestを投げてみる

```console
$ http -a admin:admin :44444/auth
HTTP/1.0 200 OK
Content-Length: 324
Content-Type: application/json; charset=utf-8
Date: Wed, 27 Dec 2017 14:27:11 GMT
Server: WSGIServer/0.2 CPython/3.6.2

{
    "accept": "*/*",
    "accept_encoding": "gzip, deflate",
    "authorization": "Basic YWRtaW46YWRtaW4=",
    "connection": "keep-alive",
    "content_length": "",
    "content_type": "text/plain",
    "host": "localhost:44444",
    "method": "GET",
    "path": "/auth",
    "querystring": "",
    "url": "http://localhost:44444/auth",
    "user_agent": "HTTPie/0.9.9"
}
```

### POSTしてみる

```console
$ http --form POST :44444/people name=foo age=20
HTTP/1.0 200 OK
Content-Length: 368
Content-Type: application/json; charset=utf-8
Date: Wed, 27 Dec 2017 14:30:20 GMT
Server: WSGIServer/0.2 CPython/3.6.2

{
    "accept": "*/*",
    "accept_encoding": "gzip, deflate",
    "body": [
        [
            "age",
            "20"
        ],
        [
            "name",
            "foo"
        ]
    ],
    "connection": "keep-alive",
    "content_length": "15",
    "content_type": "application/x-www-form-urlencoded; charset=utf-8",
    "host": "localhost:44444",
    "method": "POST",
    "path": "/people",
    "querystring": "",
    "url": "http://localhost:44444/people",
    "user_agent": "HTTPie/0.9.9"
}
```

### JSONをpostしてみる

```console
$ echo '{"name": "foo", "age": 20}' | http --json POST :44444/people
HTTP/1.0 200 OK
Content-Length: 348
Content-Type: application/json; charset=utf-8
Date: Wed, 27 Dec 2017 14:31:30 GMT
Server: WSGIServer/0.2 CPython/3.6.2

{
    "accept": "application/json, */*",
    "accept_encoding": "gzip, deflate",
    "body": {
        "age": 20,
        "name": "foo"
    },
    "connection": "keep-alive",
    "content_length": "27",
    "content_type": "application/json",
    "host": "localhost:44444",
    "method": "POST",
    "path": "/people",
    "querystring": "",
    "url": "http://localhost:44444/people",
    "user_agent": "HTTPie/0.9.9"
}
```
