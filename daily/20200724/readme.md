## python metashapeのwalkを思い出す

walkする関係を整理してみる。

- 型ヒントを一つ以上持つ必要がある
- aggressiveをつけるとmarkを自動でやる
- recursiveをつけるとnestしたクラスにも対応できる。
- (ignoreをつけるとスキップする）

### bug

- listを渡すとエラーになる現在
- 型ヒント忘れはけっこう深刻かもしれない。walkerを別途作っても良いかも？

### もう少し綺麗な単純なwalkerを考えてみる

- marker
- guess mark

これが設定できれば良い。その上でguessを手軽に把握できると良いのだけど。

### pythonを設定ファイルとして使う

- 定義 (shape)
- 設定 -> dataclasses?

## python metashapeを拡張する

markerの意味付けを登録可能にしたい？

これはなんで欲しいんだろう？metashapeの本質はshapeのsetのtransformなのだけど。
このときにtransformのためのwalkerを宣言的にしたいということかもしれない。
現状はwalker側で組み込み的に記述する感じになっている。

## python get_type_hints の強化版を作ってみる？

- tx.Annotatedからいい感じにmetadataを取り出す
- そういえば、関数に対してget_type_hintsを使うとどうなるんだろう？

↑ふつうに、inspect.fullargspecっぽい感じになった。なるほど。
tx.Annotatedからの値の抽出は、そもそも何らかの取り決めが必要なのでそこまで一般化はできないかも。

### get-type-hints? fnspec

- get type hintsで十分なのでは？
- なぜfnspecが必要になる？

shortname,nameなどを取りたい(関数名などにもアクセスしたい）。

