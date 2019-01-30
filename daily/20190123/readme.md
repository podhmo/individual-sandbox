## go go-swaggerと和解



## go json response

goでjson.Encoderを使ってresponseを生成する時にエラーになったときの対応方法がわからないかも？
http.ResponseWriterに書き込まれた瞬間にStatusは確定しちゃうのだよなー

## go test snapshot testing

testdata/xxx.Golden をつくると良いのでは？

```
SNAPSHOT_UPDATE=1 go test
```

で保存。

基本的な方針は以下

- snapshotを記録する操作をつくる
- apiのresponseは記録したものと比較する

## go test

- https://github.com/podhmo-sandbox/apiserver-examples
- https://godoc.org/github.com/google/jsonapi
- https://christina04.hatenablog.com/entry/2017/01/06/190000
