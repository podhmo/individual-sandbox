# python 内部でのrequestのアクセス先を見る

だいたい以下を網羅しておけばOK

- requests
- urllib

## requests

`requests.packages.urllib3` のdebugのlogを出力するようにする。

e.g.

```python
import requests
import logging

logging.basicConfig(level=logging.CRITICAL)  # 他のログも表示したい場合にはもっと弱めのものを指定
requests_log = logging.getLogger("requests.packages.urllib3")
requests_log.setLevel(logging.DEBUG)
requests_log.propagate = True
```

## urlib

`http.client.HTTPConnection.debuglevel` に 0以外を指定すれば良かったハズなのだけれど。

```python
import http.client

http.client.HTTPConnection.debuglevel = 1
```

現在の環境(Python 3.5.2)では、urllibのコードの問題により以下のようなmonkey patchが必要になる。

```python
from urllib.request import AbstractHTTPHandler

def __init__(self, debuglevel=None):
    self._debuglevel = debuglevel or self.__class__._debuglevel

AbstractHTTPHandler.__init__ = __init__
AbstractHTTPHandler._debuglevel = 1
```

理由は、 `urllib.request.AbstractHTTPHandler` の定義が以下の様になってしまっているため。常にhttp_classに0が渡されてしまう形になってしまう。そしてそのdefault値を強制的に変更することができない。

```python
class AbstractHTTPHandler(BaseHandler):

    def __init__(self, debuglevel=0):
        self._debuglevel = debuglevel

    def set_http_debuglevel(self, level):
        self._debuglevel = level

# ... snip

    def do_open(self, http_class, req, **http_conn_args):
        """Return an HTTPResponse object for the request, using http_class.

        http_class must implement the HTTPConnection API from http.client.
        """
        host = req.host
        if not host:
            raise URLError('no host given')

        # will parse host:port
        h = http_class(host, timeout=req.timeout, **http_conn_args)
        h.set_debuglevel(self._debuglevel)
```
