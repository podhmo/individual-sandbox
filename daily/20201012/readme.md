## docker port forwarding

localでのport forwardingを試す。

- containerで8000で開く
- localの4444と繋げる

```console
$ docker run --expose 8000 -p 4444:8000 -it --rm python:3.8-slim-buster bash

#  cd /etc
# python -m http.server
Serving HTTP on 0.0.0.0 port 8000 (http://0.0.0.0:8000/) ...
```

これで、localから繋げてみる

```console
$ docker ps
CONTAINER ID        IMAGE                    COMMAND             CREATED             STATUS              PORTS                              NAMES
5311c7304f79        python:3.8-slim-buster   "bash"              34 seconds ago      Up 33 seconds       8000/tcp, 0.0.0.0:4444->8000/tcp   affectionate_heisenberg

$ http -vv HEAD :4444
HEAD / HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Host: localhost:4444
User-Agent: HTTPie/2.2.0



HTTP/1.0 200 OK
Content-Length: 3528
Content-type: text/html; charset=utf-8
Date: Mon, 12 Oct 2020 09:27:51 GMT
Server: SimpleHTTP/0.6 Python/3.8.6
```

