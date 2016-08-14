まだうまくいっていない。

# mitmproxy or twisted ?

要望自体はmitmproxyだけで満たせる？

[Inline Scripts — mitmproxy 0.17.1 documentation](http://docs.mitmproxy.org/en/stable/scripting/inlinescripts.html)

```
mitmdump -s <script>
```

# client proxy

httpだけならproxyで作れるけれど。httpsにも対応するなら内部で自分用の証明書を使ってリレーする感じにしないとだめ。
CONNECT methodを使ったhttps proxyだとそもそも中を改変できないし。

```
$ python examples_proxy/01_echo_only_http.py&
$ python examples_proxy/02_echo_only_https.py google.com:443&
$ http -vvv --proxy http:localhost:8080 --proxy https:localhost:4433 https://google.com/
```

## 参考

- [Salmon Run: HTTP Debug Proxy with Twisted](http://sujitpal.blogspot.jp/2010/03/http-debug-proxy-with-twisted.html)
- [akashtalole/Twisted-Proxy: Twisted two way proxy for HTTP and HTTPs](https://github.com/akashtalole/Twisted-Proxy)

# ssl server

pem作る

```
openssl rsa -in server.key -text > server.pem
openssl x509 -inform PEM -in server.crt > client.pem
```

- [ssl - How to get an OpenSSL .pem file from .key and .crt files? - Stack Overflow](http://stackoverflow.com/questions/991758/how-to-get-an-openssl-pem-file-from-key-and-crt-files)
- [Using TLS in Twisted](https://twistedmatrix.com/documents/current/core/howto/ssl.html#tls-echo-server)
- [Examples](https://twistedmatrix.com/documents/current/core/examples/)

# 証明書発行

わりと忘れる。

```
$ openssl genrsa 2048 > server.key
$ openssl req -new -key server.key > server.csr
$ openssl x509 -days 365 -req -signkey server.key < server.csr > server.crt
```

## 参考

- [オレオレ証明書をopensslで作る（詳細版） - ろば電子が詰まっている](http://d.hatena.ne.jp/ozuma/20130511/1368284304)
