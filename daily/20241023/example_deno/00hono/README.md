# index.html

- https://hono.dev/docs/guides/jsx

## http server

server

```console
$ deno serve index.tsx
```

client

```console
$ http :8000/
HTTP/1.1 200 OK
content-encoding: gzip
content-length: 101
content-type: text/html; charset=UTF-8
date: Tue, 22 Oct 2024 23:27:24 GMT
vary: Accept-Encoding

<html><body><h1>Hello Hono!</h1><ul><li>Good Morning!!</li><li>Good Evening!!</li><li>Good Night!!</li></ul></body></html>
```
