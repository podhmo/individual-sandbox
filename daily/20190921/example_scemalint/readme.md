# 作業

たしかとりあえず繋げたというところまでだった記憶。

- やっぱり補完まですすめたいかも
- cueが流行ってくれて全部不要になると言うならそれはそれで

あとなんとなく使っていておもったことは

- はやいは正義
- main.yamlを開いたらフルスキャンは困る
- 既存のファイルはopenapiのフルの仕様でvalidationできないな
- そのschemaでvalidationをするのかの選択がちょっとめんどう

やろうとしてたことは

- validation部分をevent stream化

### event stream化

何かむずかしいな。どういうのがキレイなんだろう。

:feet: main()関数がrun()を引数に取れるようにしておくと便利だなー。
手抜きでstreamという名前の抽象を作ってしまった

### emacsのflycheckの設定

とりあえず動く事は確認した程度。
INFOでちょっとしたログを出してみても良いかもしれない。

これを動くようにしたとしてschemaの取得や推定部分を作らないとあんまり便利にならないなーと思ったりした。

### guess schema

yamlを指定するのかpythonのコードを指定するのかどっちが良いんだろう。
ifとかwhenとか描きたいのでpythonなきがする。
例えばopenAPIのsub部分とmain部分ではschemaは変わりそう。

どういう感じで変わるんだろう？

- 指定されたschemaでvalidationをする
- filenameから特定のファイル名のときだけ別のschemaを利用する
- 対象のファイルの中身を覗いて分岐する

#### 細かいめんどくさそうな部分

schemaをドキュメントの中身を見て決めるというタイプだとちょっとむずかしい。
with_schema()で設定する前にdocumentの中を覗くことになる。
そしてそれらはloadで失敗したときにはスキップしたい。

loggerの部分を集めるのがめんどくさいな。

### emacsサポート

- 考えてみるとltsvよりjsonになっていたほうが楽
- flycheck-parse-jsonがけっこう便利
- そしてlet-alistが強い
- filenameがないと表示されないことが分かった

### 不足していそうなこと

- lintだけの場合には何を書いて良いかわからない
- 完成形のデータからschemaが生成されて欲しい
- やっぱり補完が欲しい
- skeleton的なものが生成できると便利かもしれないい？

