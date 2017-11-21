server

```
$ make serve
$ make serve
PYTHONPATH=. python mock.py --port 33333 --transform=fullname:transform data.json
Serving on port 33333...
127.0.0.1 - - [21/Nov/2017 16:21:25] "GET / HTTP/1.1" 200 62
```

client

```
$ http -b :33333
{"firstName": "Foo", "lastName": "Bar", "fullName": "Foo Bar"}
```
