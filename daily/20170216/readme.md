# pyramid pyramidのこと復習

hello worldとか試したい。１回きりで終わらせたい場合には `handle_request()` を使うと便利。

```python
server = make_server('0.0.0.0', 8080, app)
# server.serve_forever()
server.handle_request()
```

## Acceptをつけるとjsonを返す。

```bash
$ make server &
$ make json404
{
    "code": "404 Not Found",
    "message": "The resource could not be found.\n\n\n/404\n\n",
    "title": "Not Found"
}
```

## エラーのときにはapplication/jsonになってくれない。

```bash
$ make server &
$ make json500
HTTP/1.0 500 Internal Server Error
Content-Length: 59
Content-Type: text/plain
Date: Thu, 16 Feb 2017 14:26:13 GMT
Server: WSGIServer/0.2 CPython/3.5.2

A server error occurred.  Please contact the administrator.
```

調べてみたところ、 pyramid.tweensのexcview_tweenのところで処理されている事がわかった。
単にエラーをログで出したい場合には [pyramid_exclog](https://github.com/Pylons/pyramid_exclog) を使うのが良さそう。

内部的にはこういうことしている。

```python
config.add_tween('pyramid_exclog.exclog_tween_factory', under=EXCVIEW)
```

以下の様にするとむりやり上書きできる。

```python
def exc_view(request):
    tbio = io.StringIO()
    traceback.print_exception(*request.exc_info, file=tbio)
    return {"message": str(request.exception), "traceback": tbio.getvalue()}

config.add_view(view=exc_view, context=Exception, renderer="json")
```


# python abc abcどこまでやってくれたっけ？


```python
import abc


class Base(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def foo(self):
        pass


class A(Base):
    pass

a = A()  # error
```

一応、インスタンス生成時にチェックはしてくれるみたい。

# python pyramid rest api

この辺知らなかった

- https://github.com/ramses-tech/nefertari
- https://github.com/ramses-tech/ramses

昔からある

- https://github.com/Cornices/cornice

知りたかったのはHTTPExceptionのresponseの形式だけだった。

## 雑に開発するだけで良いというなら

pyramidのhttpexceptions.pyを除いてみたらprepare()のところでHTTP_ACCEPT見てる。これがapplication/jsonだった場合にはjsonになるっぽい。
stack traceの有無的なことを確認したかったり。

ACCEPTヘッダー渡す。
ただし、internal server errorのときには自動的にtext/plainになってしまう？

## responseの仕様

と言うかこの辺見ておけば良さそうな感がある。

- [HTTP APIの詳細なエラー情報をレスポンスに持たせるための仕様](https://www.eisbahn.jp/yoichiro/2017/01/rfc_7807.html)
- [WebAPIでエラーをどう表現すべき？15のサービスを調査してみた - Qiita](http://qiita.com/suin/items/f7ac4de914e9f3f35884)


