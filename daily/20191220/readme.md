## fastapi 暇つぶしのデバッグ

https://github.com/bloomingmath/fastapi-middleware-issue

```
        print("@", message, message["type"], "@@")
```

output

```
main.py::test_middleware @ {'type': 'http.response.template', 'template': <Template 'page.html'>, 'context': {'request': <starlette.requests.Request object at 0x7f9f2ced5850>}} http.response.template @@

...
>       assert message["type"] == "http.response.start"
E       AssertionError

../lib/python3.8/site-packages/starlette/middleware/base.py:48: AssertionError
===================================================== 1 failed, 1 passed in 0.31s ======================================================
m
```

どうも 0.13.0 では直ってるっぽい。そして0.12.14で発生している。ということはコレのどれか？
(fastAPIでインストールされるものは0.12.9?)

## 追記

原因は分かった。TestClientのための特別扱いをしているせい。

https://github.com/encode/starlette/blob/00e830080499c47a3e72f1b4adb2678758e388ee/starlette/templating.py#L30-L41

requestからextensionsを取り出した時に "http.response.template" を含んでいたら特殊な処理をしている。

```py
class _TemplateResponse(Response):

# ...
   async def __call__(self, scope: Scope, receive: Receive, send: Send) -> None:
        request = self.context.get("request", {})
        extensions = request.get("extensions", {})
        if "http.response.template" in extensions:
            await send(
                {
                    "type": "http.response.template",
                    "template": self.template,
                    "context": self.context,
                }
            )
        await super().__call__(scope, receive, send)
```

そして、starlette.TestClientを使った場合とasyc_asgi_testclient.TestClientを使った場合とでrequest (scope) の値が異なる。

starlette

```
scope={'type': 'http', 'http_version': '1.1', 'method': 'GET', 'path': '/', 'root_path': '', 'scheme': 'http', 'query_string': b'', 'headers': [(b'host', b'testserver'), (b'user-agent', b'testclient'), (b'accept-encoding', b'gzip, deflate'), (b'accept', b'*/*'), (b'connection', b'keep-alive')], 'client': ['testclient', 50000], 'server': ['testserver', 80], 'extensions': {'http.response.template': {}}, 'app': <starlette.applications.Starlette object at 0x7f7806e77970>}
```

async-asgi-testclient

```
scope={'type': 'http', 'http_version': '1.1', 'asgi': {'version': '3.0'}, 'method': 'GET', 'scheme': 'http', 'path': '/', 'query_string': b'', 'root_path': '', 'headers': [(b'remote-addr', b'127.0.0.1'), (b'user-agent', b'ASGI-Test-Client'), (b'host', b'localhost')], 'app': <starlette.applications.Starlette object at 0x7f195df06700>}
```

そして、`'extensions': {'http.response.template': {}}` をどこで付けているかと言うと。

testclientの_ASGIAdapter

```py
class _ASGIAdapter(requests.adapters.HTTPAdapter):
    def __init__(self, app: ASGI3App, raise_server_exceptions: bool = True) -> None:
        self.app = app
        self.raise_server_exceptions = raise_server_exceptions

    def send(  # type: ignore
        self, request: requests.PreparedRequest, *args: typing.Any, **kwargs: typing.Any
    ) -> requests.Response:

# ...

       scope = {
            "type": "http",
            "http_version": "1.1",
            "method": request.method,
            "path": unquote(path),
            "root_path": "",
            "scheme": scheme,
            "query_string": query.encode(),
            "headers": headers,
            "client": ["testclient", 50000],
            "server": [host, port],
            "extensions": {"http.response.template": {}},
        }
```
