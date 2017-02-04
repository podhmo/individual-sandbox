# python proxy2

https://github.com/inaz2/proxy2

こちらはopensslでCA作ってるのですぐにMITMできる。

- http://qiita.com/hitoshi_ichikawa_i3/items/cd2f6b0477482a358929

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

# golang httputilが便利そう

- reverse proxyも簡単にかける

ただしhttpまで。httpsにはman in the middleみたいなことしないとダメ。goproxyが便利そう。

## memo

- http://blog.charmes.net/2015/07/reverse-proxy-in-go.html
- https://github.com/elazarl/goproxy
- https://github.com/containous/traefik
