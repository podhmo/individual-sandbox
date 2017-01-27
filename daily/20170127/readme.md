# golang go-swagger go-swaggerで生成したclientを実行する時には `DEBUG=1` を付けると良い

以下の様な感じに。するとrequestとresponseの情報が少しだけ出力される。便利。

```bash
$ DEBUG=1 go run client.go
# あとでsample outputを貼る
```

内部的には [net/http/httputil](https://golang.org/pkg/net/http/httputil/) が使われる。
