## go context

WithTimeoutとcancelを付けたclientを生成しても良いのではないか？

## go backoff

そういえばbackoffって何が良いんだろう？
結局health check 用のAPIを作ってそこに内部から叩くのが一番楽な気がしてきた。

## go fiber

fiberをいじってみるか。

- https://github.com/gofiber/fiber
- https://github.com/gofiber/adaptor
- https://github.com/gofiber/fiber/blob/master/middleware/logger.md
- https://github.com/gofiber/fiber/blob/master/middleware/request_id.md

APIの実行などを一括でやりたい。以前egoistをいじっていたときの作業が使えると嬉しい。

あと、adaptorでhttp.Handlerを変えられる。
気にしたいのはapidocの生成かな。

### adaptor

- https://github.com/gofiber/adaptor

`adaptor.HandlerFunc()`を使って変換しているっぽい。

内部的には `fiber.New()` でappを作って、AcqireCtx,ReleaseCtxを呼んでおしまいくらいっぽい？
実際には`net/http.Request`を作って、bodyに注入して実行しているっぽい。けっこう無理矢理っぽい感じ。

fiberはRequestPoolというところにpoolオブジェクトを貯めているっぽい。
一応動ける程度な感じかもしれない。

fasthttpのRequestCtxを使うと言う感じっぽい。
いろいろ読んだけど、個別に使うのもおかしいし、本当にsubrouter的な感じで使うものっぽいな。

### web socket用のhello worldを考えたい

### openapi

https://github.com/arsmn/fiber-swagger
