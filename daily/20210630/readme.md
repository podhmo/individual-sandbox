## go forward proxy

- https://github.com/kazeburo/chocon

## go x/sync

たしかいい感じのユーティリティがあった記憶。singleflightか。

https://pkg.go.dev/golang.org/x/sync@v0.0.0-20210220032951-036812b2e83c/singleflight

しかし、こちらは同じ結果を返すような実装だ。

[x/sync/singleflight の注意点とゼロタイムキャッシュ - Qiita](https://qiita.com/methane/items/27ccaee5b989fb5fca72)

[singleflight でキャッシュのOriginへのリクエストを抑制 - Carpe Diem](https://christina04.hatenablog.com/entry/go-singleflight)


欲しいものは重複部分を429か409を返すようなものな気がする。throttleみを感じるのは429。
ただ、他と衝突しなそうなのが409なきがする。変なbackoffに吸い込まれない。

## go throttle proxy

特にそのようなOSSはないのか。自分で作らなくちゃ行けなそう。
そんなに難しいものではないような気がする？

```console
$ WEBHOOK_URL=https://hooks.slack.com/services/T00000000/B00000000/XXXXXXXXXXXXXXXXXXXXXXXX
$ throttle-proxy --port 444444 --register xxx="${WEBHOOK_URL}"
```

こんな感じで使う。そして以下のようにリクエストすると、対応するwebhookにrequestする。

```
POST :44444/xxxx {"channel": "random", "message": "hello"}
```

通常は素直にproxyする。requestが連打された場合には少な目にrequestする。
slack単体のものにするか任意のものに対するproxyにするかは悩ましいところではある。

429(Too many request)か409(Conflict)を返すイメージだった。

### go proxy

- goのproxyでreverse proxy用に特別なものってあるのだっけ？

```go
h = httputil.NewSingleHostReverseProxy(url)
```

もしくは

```go
h = &httputil.ReverseProxy{
    director: func(req *http.Request){
        // do something
    }
}
```

- :memo: req.Clone(ctx) で複製できたんだ ([net/http: Request.Clone() does not deep copy Body contrary to its docs (using GetBody works though) · Issue #36095 · golang/go](https://github.com/golang/go/issues/36095))。 
- errorを返す方法がないかもしれない。
- transport.RoundTrip()を実行しているだけ
- (全然関係ないけど、 `io.Copy(ioutil.Discard, input)` ってどこでやるんだっけ？)

    - https://mattn.kaoriya.net/software/lang/go/20160329094503.htm この話か。
    - [This 3 year old thread had an important dicussion about how and when to close a response.body [Do i need to read the body before close it?] How does it stand today? : golang](https://www.reddit.com/r/golang/comments/fil647/this_3_year_old_thread_had_an_important_dicussion/)
    - [Do i need to read the body before close it? - Getting Help - Go Forum](https://forum.golangbridge.org/t/do-i-need-to-read-the-body-before-close-it/5594)   
    -  どうやらleakというよりは、connectionが再利用されないみたいな話っぽい。ただし2017年の話だ。

### 備考

- なるべくdockerfileを書かずに済ませたい
- コマンドライン引数や環境変数で済ませたい
- 動的な方が嬉しいんだろうか？静的なもので十分では？
- proxy先がconnectionを切ったときには499を返したい

URL

- [Goでproxy serverを作るときにハマるポイント | メルカリエンジニアリング](https://engineering.mercari.com/blog/entry/2018-12-05-105737/)
- [httputil - The Go Programming Language](https://golang.org/pkg/net/http/httputil/)
- [go/reverseproxy.go at master · golang/go](https://github.com/golang/go/blob/master/src/net/http/httputil/reverseproxy.go)
- [GoのWebアプリケーションでステータスコード499を記録する | おそらくはそれさえも平凡な日々](https://songmu.jp/riji/entry/2020-12-16-go-http499.html)

:thought-balloon: transportを渡してアレコレするしかなさそうだな。

### go httputilのReverseProxy

- hop by hop header
- X-Forward-For

[Goでリバースプロキシつくるときにつかえる net/http/httputil.ReverseProxy の紹介 - Qiita](https://qiita.com/convto/items/64e8f090198a4cf7a4fc)
[コードリーディング: Go 標準ライブラリ net/http/httputil ReverseProxy での WebSocket の取り扱い - エムスリーテックブログ](https://www.m3tech.blog/entry/go-handling-of-websocket-in-reverseproxy)

#### hop-by-hop header?

- https://datatracker.ietf.org/doc/html/rfc2616#section-13.5.1

proxy先に見せないヘッダーのことみたい？単純に削除するだけで良いの？

- Connection
- Keep-Alive
- Proxy-Authenticate
- Proxy-Authorization
- TE
- Trailers
- Transfer-Encoding
- Upgrade


## python pep660

acceptedされた。

https://www.python.org/dev/peps/pep-0660/

