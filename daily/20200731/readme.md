## なんとなくUMLのクラス図などを

- https://fsteeg.wordpress.com/2006/11/17/uml-class-diagrams-with-graphviz/
- http://www.ffnn.nl/pages/articles/media/uml-diagrams-using-graphviz-dot.php

そもそも、どういう矢印があるかとか思い出せていないな。graphvizのやつ。
あと、なんか特殊な書き方をしている気がする。

この辺はgraphvizの話

- https://qiita.com/rubytomato@github/items/51779135bc4b77c8c20d
- https://stackoverflow.com/questions/2476575/how-to-control-node-placement-in-graphviz-i-e-avoid-edge-crossings



### どうして向きが逆？

通常 `B -> A` として書くところを全部 `A -> B` にして `edge[dir=back, arrowtail=empty];`にしている。これは配置の問題っぽい。`B extends A`の元、継承元を上にしたいということらしい。 `A->B`先に現れたものの方を先に描こうとするので。


### どうやって四角い形をつくってる？

shapeをrecordにしている。labelの中に直接文字列を入れている。`{}`でテーブルが作れて `|`で区切り線を入れられるよう。`\l`は改行。

`label = "{Animal|+ name : string\l+ age : int\l|+ die() : void\l}"`

結局HTMLタイプの表現にしないと `constraint=false` のトリックが動かないらしい

- https://graphviz.org/doc/info/shapes.html

## クラス図の描き方

- https://cacoo.com/ja/blog/how-to-write-class-diagram/
- https://ja.wikipedia.org/wiki/%E3%82%AF%E3%83%A9%E3%82%B9%E5%9B%B3
- http://www.itsenka.com/contents/development/uml/class.html
- http://objectclub.jp/technicaldoc/uml/umlintro2
- https://qiita.com/devopsCoordinator/items/213e45694dfac0edcfbc

cacooのやつだけで良い気もする

hmm

- 名前 (`名前` と `<<interface>> 名前`　という書き方)
- 属性 ( `属性名：型＝初期値` )
- メソッド ( `操作名（引数：引数の型）：戻り値` )

という順で書く (ところでpropertyはメソッド?)

#### 可視性

属性やメソッドに可視性を追加できる

- `+ foo` は public
- `# foo` は protected
- `- foo` は private
- `~ foo` は package

まぁこの辺は無視して良さそう

#### 関係

- リンク

  - 関連 association has-a ( `----` , `--->`, `<---`)

    - 集約 aggregation part-of  ( `<>---` )
    - コンポジション composition part-of (`<>---` 中は塗りつぶし)


- 依存 dependency (`<- - -`)
- 汎化 generalization (`<|---`)
- 実現 realization (`<|- - -`)


図示するときにまじで書くときには

- 左上 多重度
- 左下 ロール
- 真ん中 関連名
- 右上 多重度
- 右下 ロール

という感じにラベルを付与する
