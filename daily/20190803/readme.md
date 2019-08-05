## go go-webtest

clientを実装するか。

その前にhttpbinみたいな何かを用意するとよいかも。

### 追記

httpbinどこに置こう。とりあえずgo-webtest/httpbinで良いか。
コマンドとして動作チェックもしたいな。go-webtest/cmd/httpbinで良いか。

### 追記

やっぱり現時点でもテストを書いておいたほうが良いか。

1. noerrorを使おう
1. ついでにgo-webtest/jsonequalも使おう

errorを返す比較関数が何でも使えるのは便利だな。

### 追記

そろそろまじめにclientを作るか。とりあえず以前書いたのをコピーしよう。
[../20190731/example_gowebtest/myapi](../20190731/example_gowebtest/myapi)の部分。

あと何を気にしたいのだっけ？

- Get以外のメソッドに対応したいかも
- Get以外のメソッドを生やす便利struct作る？
- middleware的なものを用意したいかも。RoundTripperにまかせる？
- net/httpのReadResponseはなにかに使えないかな。

これだけでDELETEとかはないのか。

```
func Get(url string) (resp *Response, err error)
func Head(url string) (resp *Response, err error)
func Post(url, contentType string, body io.Reader) (resp *Response, err error)
func PostForm(url string, data url.Values) (resp *Response, err error)
```

### 追記

テストも使った形に書き換える？
ふと、これは思いつきだけれど、client自体をio.ReadCloserにしてあげればきれいにお片付けできたりするのでは？(本当？）

あと、ForRecorderというよりForHandlerなきがした。

### 追記

とりあえずそれっぽい感じにした。変更したいのは何だろう？

- methodの整理
- middleware

  - round tripper?
  - snapshot?

:thought_balloon: RoundTripperのテスト手軽にやりたくない？

- GET,POST,PUTみたいなメソッド要ら無くない？

### 追記

middlewareぽいものを追加した。

- middlewareというなまえが良くない気がする
- middleware部分のエラーのレイアウトを良い感じにしたいな。。
- requestをmodifyする部分パッケージ分けられないかな。

  - request factoriesみたいな感じ？

- context受け取るようにしておきたい

  - x/tools/net/ctxthttpのsignature特にきれいでもなかったな。

snapshotのmetadataの話

- 誰が作ったか(generatedBy)的なものを記録したほうが良いかも？

  - あとで生成の入力に使いたい

- あと省きたいこともあるかも。

### 追記

やりたいことが発散しているかも。とりあえず基礎的なことができる様にしよう。

- capture的なもの
- snapshot test的なもの

そういえば、httpbinではなくgo-webtestのintegration testということにすると良いのでは？

### 追記

- snapshot testでのデータの受け渡しに `*interface{}`を使うことになったのだけれど。キモいな。

そろそろ別の場所でコードを書いて使ってみるか。。


## go handy -> noerror

テキトーにあれこれして記事を書いた。
やっぱり文章書くと時間が掛かるな。。
