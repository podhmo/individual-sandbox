# python dictのwalker

# python xmlrpc

そういえばxml-rpcのtutorialのやつ。メモっていなかったので[メモった](../20161013/example_xmlrpc)

# python wsgiref

python3の方だとhttp.serverの裏側の実装が [selectors](http://docs.python.jp/3/library/selectors.html) モジュール使っているっぽい？
kqueue,epoll使っているということ。

正確には以下のような依存関係。

```
wsgiref.simple_server -> http.server -> socketserver -> selectors
```



