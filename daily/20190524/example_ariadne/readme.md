setup

```
# error
# pip install --upgrade uvicorn ariadne

$ pip install --upgrade starlette uvicorn ariadne
```

run

```console
$ make run
# access http://localhost:8000

ERROR:uvicorn:Exception in ASGI application
Traceback (most recent call last):
  File "VENV/lib/python3.7/site-packages/uvicorn/protocols/http/httptools_impl.py", line 368, in run_asgi
    result = await app(self.scope, self.receive, self.send)
  File "VENV/lib/python3.7/site-packages/ariadne/asgi.py", line 54, in __call__
    await self.handle_http(scope=scope, receive=receive, send=send)
  File "VENV/lib/python3.7/site-packages/ariadne/asgi.py", line 73, in handle_http
    await response(scope, receive, send)
TypeError: __call__() takes 3 positional arguments but 4 were given
```

## starlette is also old version

ariadne/asgi.py

```python
class GraphQL:
# ...
    async def handle_http(self, scope: Scope, receive: Receive, send: Send):
        request = Request(scope=scope, receive=receive)
        if request.method == "GET":
            response = await self.render_playground(request)
        elif request.method == "POST":
            response = await self.graphql_http_server(request)
        else:
            response = Response(status_code=405)
        await response(scope, receive, send)
```

starlette/responses.py

```python
class Response:
    media_type = None
    charset = "utf-8"
# ...
    async def __call__(self, receive: Receive, send: Send) -> None:
        await send(
# ...
```

hmm

```
$ pip list --outdated | grep starlette
starlette                0.9.9                 0.12.0      sdist
$ pip install --upgrade starlette
```

## ok

server

```console
$ make
uvicorn myscript:app
INFO:uvicorn:Started server process [31539]
INFO:uvicorn:Waiting for application startup.
INFO:uvicorn:ASGI 'lifespan' protocol appears unsupported.
INFO:uvicorn:Uvicorn running on http://127.0.0.1:8000 (Press CTRL+C to quit)
```

client

```console
$ echo '{"query":"{hello}"}' | http --json POST :8000
HTTP/1.1 200 OK
content-length: 41
content-type: application/json
date: Thu, 23 May 2019 16:07:53 GMT
server: uvicorn

{
    "data": {
        "hello": "Hello, HTTPie/1.0.2!"
    }
}
```
