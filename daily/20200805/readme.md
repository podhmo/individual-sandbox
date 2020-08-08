## python class diagram

ちょっとだけお試しで作ってみる。

考えてみると以下の様なことが増える。

- いい感じのグラフを作る
- 関係性に対して描写する
- 関連の数値に対して描写する

ER図が欲しい場合と何が違うんだろうか？というようなことは考えても良い。
クラス図で欲しくなるのは関連？

なんでもgraphvizに頼るのもと思いつつ、metashapeにクラス図の生成機能があっても良いのかもなーと思い始めた。pyreverseじゃなぜだめか？みたいな話もある。

### 追記 いい感じのグラフ

- [../20200731/example_viz/06animals.dot](../20200731/example_viz/06animals.dot)
- https://cacoo.com/ja/blog/how-to-write-class-diagram/
- https://qiita.com/ykawakami/items/f6688b845945669f0ce5
- https://note.com/kitabatakep/n/n81582e3b4adf
- http://www.aise.ics.saitama-u.ac.jp/~gotoh/ClassDiagramByPlantUML.html


### 追記

関連の部分をいい感じに表すことができない
関連名がクラス定義に現れない

### 追記

関係性をどうやって表そうか

- `n`
- `0,n` or `*`
- `1..*`
- `m..n`


- リンク

  - 関連 association has-a ( `----` , `--->`, `<---`)

    - 集約 aggregation part-of  ( `<>---` )
    - コンポジション composition part-of (`<>---` 中は塗りつぶし)


- 依存 dependency (`<- - -`)
- 汎化 generalization (`<|---`)
- 実現 realization (`<|- - -`)


### 追記

英語名は列挙できても良い気がする

- association
- inheritance
- realization
- dependency
- aggregation
- composition

あとcardinality

- exactly one
- zero or one
- zero or more
- one or more

### 追記



