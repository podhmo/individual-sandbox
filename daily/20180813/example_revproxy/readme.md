```
$ make server&
[1] 27748
python server.py --port 5000
$ make proxy&
[2] 27754
python proxy.py --port 5001
$ make client
http :5001/api/foo
127.0.0.1 - - [14/Aug/2018 00:12:29] "GET /api/foo HTTP/1.1" 200 28
127.0.0.1 - - [14/Aug/2018 00:12:29] "GET /api/foo HTTP/1.1" 200 36
HTTP/1.0 200 OK
Content-Length: 36
Content-type: application/json; charset=utf-8
Date: Mon, 13 Aug 2018 15:12:29 GMT
Server: WSGIServer/0.2 CPython/3.7.0

{
    "age": "**20**",
    "name": "**foo**"
}
```
