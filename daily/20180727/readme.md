## pip proxy経由で中覗く方法

```
pip --proxy localhost:8080 --cert proxy2/examples/ca.crt search <package>
```

## arch locate

```
sudo pacman -Sy mlocate
sudo updatedb
locate <pattern>
```

## certificates

- https://qiita.com/kunichiko/items/12cbccaadcbf41c72735
- https://www.ecoop.net/memo/archives/guide-for-pem.html

## reverse proxy python

## reverse proxy go

- httputil.ReverseProxy
- httputil.NewSingleHostReverseProxy

https://www.integralist.co.uk/posts/golang-reverse-proxy/

## header

- X-Forwarded-Host
- X-Origin-Host

まじめにかくと

- X-Forwarded-For (ip address)
- X-Forwarded-Proto (protocol)
- X-Forwarded-Host (hostname)
- X-Forwarded-Port (port)

## そういえば

https://github.com/inaz2/proxy2
https://www.slideshare.net/inaz2/httpproxy2

### 追記

```console
$ python2 proxy2/examples/sslstrip.py
...
  File "/usr/lib/python2.7/socket.py", line 228, in meth
    return getattr(self._sock,name)(*args)
socket.error: [Errno 99] Cannot assign requested address
```

実際中を覗いてみると、

```
(Pdb) self.address_family, self.socket_type
(10, 1)
```

どうも、ipv6の方が使われている模様。

```console
$ python -m pydoc socket
...
AddressFamily
     |  AF_INET = <AddressFamily.AF_INET: 2>
     |  
     |  AF_INET6 = <AddressFamily.AF_INET6: 10>
SocketKind
    SOCK_CLOEXEC = <SocketKind.SOCK_CLOEXEC: 524288>
    SOCK_DGRAM = <SocketKind.SOCK_DGRAM: 2>
    SOCK_NONBLOCK = <SocketKind.SOCK_NONBLOCK: 2048>
    SOCK_RAW = <SocketKind.SOCK_RAW: 3>
    SOCK_RDM = <SocketKind.SOCK_RDM: 4>
    SOCK_SEQPACKET = <SocketKind.SOCK_SEQPACKET: 5>
    SOCK_STREAM = <SocketKind.SOCK_STREAM: 1>
```

げんいんはこれ？(ipv6じゃん)

```
server_address = ('::1', port)

class ThreadingHTTPServer(ThreadingMixIn, HTTPServer):
    address_family = socket.AF_INET6
    daemon_threads = True
```

### hmm

```
http --cert server.cer --cert-key server.key --verify ca.pem https://self.sample.com/
```

hmm

```
http: error: SSLError: HTTPSConnectionPool(host='pod.hatenablog.com', port=443): Max retries exceeded with url: / (Caused by SSLError(SSLError(1, '[SSL: CERTIFICATE_VERIFY_FAILED] certificate verify failed (_ssl.c:777)'),)) while doing GET request to URL: https://pod.hatenablog.com/

$ ALL_PROXY=http://localhost:8080 curl -k https://pod.hatenablog.com/
```

証明書のあたりでだめっぽい。firefoxでも `SEC_ERROR_UNKNOWN_ISSUER` とか出る。

```
Your connection is not secure

The owner of pod.hatenablog.com has configured their website improperly. To protect your information from being stolen, Firefox has not connected to this website.

This site uses HTTP Strict Transport Security (HSTS) to specify that Firefox may only connect to it securely. As a result, it is not possible to add an exception for this certificate.
```

```
pod.hatenablog.com uses an invalid security certificate. The certificate is not trusted because the issuer certificate is unknown. The server might not be sending the appropriate intermediate certificates. An additional root certificate may need to be imported. Error code: SEC_ERROR_UNKNOWN_ISSUER 
```

firefoxのroot caに入っていないやつだとダメっぽい感じ？mitmdumpでもなるな。

### 追記

ブラウザの設定で Certificates > View Certificates > Authorities > Importで

setup_https_intercept.shしたあとのca.crtを入れれば大丈夫。
(mitmproxyの場合も~/.mitmproxy/mitmproxy-ca-cert.cer を)

### httpie でやる場合

```
http --cert <cert.crt> [ --cert-key <client.key> ] ...
```


わりと丁寧

- https://githubja.com/jakubroztocil/httpie

## mitmproxy

```
pip install mitmproxy
```

mitmdump,mitmweb,mitmproxyが入る

```console
# mitmdump --anticache --ssl-insecure -p 8080
$ mitmdump --anticache -p 8080
```

~/.mitmproxy/mitmproxy-ca-cert.cer を見ないとなのか

### mitmproxyのmodeについて把握しておきたい
