## metashape 今日はけっこう近くをなおそう

- required付きでmetadataを表示したい？
- そもそもdefaultを見てなさそう
- metadataを指定可能にしておきたいかも
- pathの生成のDSLを考えたい
- resolver周りを綺麗にしたい
- memberの名前変えたい

### metadataの対応

まだ対応していなかったっけ？
できた。ついでにextra_metadataも。resolver周りのリファクタリング大切かもしれない。

### pathの生成のDSLを考えたい

### resolver周りを綺麗にしたい

そもそもresolverってなんだろう？ということの答えが出ていなかった。
なんとなく各outputsのemitで直接コードを書いたりdetect.pyに関数を書いたりするのと何が違うのかという答が出ていなかった。


いろいろ考えてみたんだけれど、特にoutputのcontextなどに含まれないdataの取得がresolver経由になるべきという結論になりそう。例えばデフォルト値とか。

### ちょこっとだけgoの生成を見てみる

- 複数ファイルを出力できるようにしないとかも
- パッケージを指定できないとまずい
- タグの部分を自分達独自のものにしようとすると。。
- goimportsどうやって実行しよう。。
- どうやってemitするかをdesignで記述するのが正しそう。

hmm

- goで生成するのはstructが嬉しいんだろうか？
- 他の部分も含めてな気がする


macroっぽさ

- additionalProperties:falseやrequired
- default error response
