# python httpieで `--ssl ssl3` にするとエラーになる

```
$ http --debug --ssl=ssl3 https://api.github.com/users/podhmo
requests.exceptions.SSLError: [SSL: SSLV3_ALERT_HANDSHAKE_FAILURE] sslv3 alert handshake failure (_ssl.c:590)
```

# hmm

- https://github.com/gevent/gevent/issues/719

```python
import socket, ssl
sock = socket.create_connection(("example.org", 443))
sock = ssl.wrap_socket(sock)
sock.send(b"")
```

```python
import socket, ssl
sock = socket.create_connection(("example.org", 443))
sock = ssl.wrap_socket(sock)
sock.sendall(b"")
```
# python proxy2

https://github.com/inaz2/proxy2

こちらはopensslでCA作ってるのですぐにMITMできる。

- http://qiita.com/hitoshi_ichikawa_i3/items/cd2f6b0477482a358929

なんかできなくなった。

以下のようなエラーが出る。

```
SSLEOFError: EOF occurred in violation of protocol (_ssl.c:590)
```

httpieでもPROTOCOL_SSLv3を指定したらだめになった。

```
$ http --ssl ssl3 GET "https://qiita.com/api/v1/search?q=go"

http: error: SSLError: [SSL: SSLV3_ALERT_HANDSHAKE_FAILURE] sslv3 alert handshake failure (_ssl.c:590) while doing GET request to URL: https://qiita.com/api/v1/search?q=go
```

# golang goproxy使ってみる

example見たほうが便利そう

- https://github.com/elazarl/goproxy/blob/master/examples/goproxy-httpdump/httpdump.go

```
$ go get -v github.com/elazarl/goproxy/examples/goproxy-httpdump
$ goproxy-httpdump
# http --follow --proxy=http://localhost:8080 "https://qiita.com/api/v1/search?q=go"
# httpsは表示できない
```

MITMの例

- https://github.com/elazarl/goproxy/blob/de25c6ed252fdc01e23dae49d6a86742bd790b12/examples/goproxy-eavesdropper/main.go

なんか設定が必要っぽい？

真面目に調べる。handshakeで失敗しているっぽい。そもそもTLSの知識がない。

- [TLS with Go](https://ericchiang.github.io/post/go-tls/)
- [A Let's Encrypt Client for Go](https://ericchiang.github.io/post/go-letsencrypt/)

## TLS

簡単なTLSサーバーを書いてみる

- https://gist.github.com/denji/12b3a568f092ab951456
- http://www.kaihag.com/https-and-go/

# golang httputilが便利そう

- reverse proxyも簡単にかける

ただしhttpまで。httpsにはman in the middleみたいなことしないとダメ。goproxyが便利そう。

## memo

- http://blog.charmes.net/2015/07/reverse-proxy-in-go.html
- https://github.com/elazarl/goproxy
- https://github.com/containous/traefik
