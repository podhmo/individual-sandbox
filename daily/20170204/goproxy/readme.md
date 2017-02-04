まだgoproxyでうまくMITMできていない。

```
const (
0	ConnectAccept = iota
1	ConnectReject
2	ConnectMitm
3	ConnectHijack
4	ConnectHTTPMitm
5	ConnectProxyAuthHijack
)
```

以下の様な形で通信

```
HTTP_PROXY=http://localhost:8080/ go run 00get/main.go
```

ログ

```
2017/02/04 21:17:54 [001] INFO: Running 1 CONNECT handlers
2017/02/04 21:17:54 [001] INFO: on 0th handler: &{2 <nil> 0xcacc0} qiita.com:443
2017/02/04 21:17:54 [001] INFO: todo action -- 2
2017/02/04 21:17:54 [001] INFO: Assuming CONNECT is TLS, mitm proxying it
2017/02/04 21:17:54 [001] INFO: signing for qiita.com
2017/02/04 21:17:54 [001] WARN: Cannot handshake client qiita.com:443 EOF
```

tlsのhandshakeで失敗しているっぽい。

http://stackoverflow.com/questions/34823724/golang-tls-handshake-error


