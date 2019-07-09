## dictknife stream

- 名前を変えよう
- 前回の続き

前回の場所はどこだったっけ？ [../20190512/readme.md](../20190512/readme.md)

### 新しい名前

openapi-streamにした。後で変えるかも。
そういえば、korpokkur捨てたいかもなー。coockier-cutterに寄せていきたい。

### やること

- nestしたときのarrayがおかしい
- たぶん存在していないrefが現れることがあるoneOfの時に暗黙の定義をしていた場合
- additionalPropertiesの候補はpropertiesとpatternPropertiesに含まれなかったもの

### やっていること

- 色々思い出す
- resolve propertiesにするかコードに直に記述するかを考える

思ったこと

- primitiveな型へのvisitorが大きいかも
- patternPropertiesの名前にpythonで扱えない文字列が使われている
- reserved wordに対応していない

### やる

- patternPropetiesをpythonで使えるものに
- <missing>をなくす




## これ暇つぶしにやる？

- http://www.cl.ecei.tohoku.ac.jp/nlp100/
- https://github.com/rougier/numpy-100

## python 良い感じのデータセットを扱う演算

- filter
- select, project, map

- join (merge)
- concat
- group by

## python 良い感じのdiff

## emacs elscreenの名前の表記がおかしい

## python join的な演算を作る

- 結合するためのkeyを指定できる(on,left_on,right_on)
- 各種joinのstrategyが選べる(inner join,left outer join, right outer join, full outer join)
- 名前の衝突を防ぐための機構が存在している
- 不足したkeyを補う機構が存在している
- 複数のkeyを組み合わせてjoinができる
- joinのkeyをpopする

名前の衝突があったらruntime error?

### joinをtupleでの合成と考えるととても実装が楽

- 名前の衝突気にしなくて良い
- 欠損フィールドをzero値で埋めなくて良い

### iteratorだけで済ませられる？

小さい方のグループだけcacheで持つみたいなことをしようとした時に `len()` が必要になってしまう。

### 表示を良い感じにしたい

- diffを確認するのはツライ
- 左右に併記する表示が便利
- おかしな箇所がハイライトされていると嬉しい
- 幅が長くなりすぎると無理
- 分割した場合には区切りがわかりにくくなる


