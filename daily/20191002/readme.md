## go ちょっとしたコメントへの反映

- openAPI doc
- operationID
- 型

とりあえず、testing辺りを参考にメソッドを集める。
そのあと、コメント部分を見ていく、埋める。

### load

load部分どうしようかな。go/packagesあたり使えるんだろうか？

- https://godoc.org/golang.org/x/tools/go/packages

無理ならいつもどおりloaderで

- https://godoc.org/golang.org/x/tools/go/loader

### loadの使いかた

[loadMode](https://godoc.org/golang.org/x/tools/go/packages#LoadMode)を指定するのは大切そう。

exampleはこの辺。

- https://godoc.org/golang.org/x/tools/go/packages#example-package

importの依存も含めて読み込むならVisitが便利そうだった。
指定したものだけを見るならとりあえずsyntaxをloopするだけで大丈夫そう。

あとは何だろう？[go/ast](https://godoc.org/pkg/go/ast)ベースでメソッドを取り出せばよいのか。

### 取得

- ファイル名の取得
- メソッドの取得
- メソッドのreceiverの取得

そして表示用のprinterがめんどくさいのだった
(godocの中を覗くのが楽？)

- :thought_balloon: 埋め込みまで考えると素直にgo/typesなどベースの方が楽
- :thought_balloon: かんたんなutilityがあると良いかも？

### 表示

godocの実装を覗くか。


## python schemalint

- ../20190928/example_schemalint/readme.md
