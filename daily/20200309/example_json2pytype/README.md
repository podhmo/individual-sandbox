# json to x

JSONをscanして取り回しの良い形にしたい

- nodeごとにflatにiterateできる
- node毎にタイプが推測できる

## commands

テキトーなコマンドがほしい

- scan
- emit

`--with-annotation`

### annotations

typeの名前などを固定したい。
このときのためにannotation fileのようなものを渡したい。
が、これを手動で書くことは避けたい。というわけでこれも出力できるオプションが欲しい。

TODO: もう少し良い名前

```console
$ annotations emit data.json > annotations.json
$ emit data.json --with-annotations=annotations.json
```


## union/required,unrequired

以下の機能もやっぱり標準で欲しいなー。

- required,unrequired -- オブジェクトに階層関係
- union -- オブジェクトに分岐

とりあえずunionは置いておいてrequired,unrequiredはほしい

### reuired/unrequired

方針としてはフィールドの統計値を取れば良いはず。
nullをどう扱うかは話としてあるかもしれない。

真面目に考えるとこういう階層関係。

- `(?x, y) > (x, y) > (x,)`
- `(x, y)`, `(x, z)` から `(x, ?y, ?z)` をつくる

maxと常に比較していけば良い。

#### primitive typeや container typeがやってきた場合

これがめんどくさいな。一旦はruntime errorにしちゃうか。

### 巨大なデータ

巨大なデータをフルでparseするのはバカバカしいか。
head,tailの数値を付けたいかも？

## detectという関数にしたい

一部分だけをテスト可能にしたい。

## many

複数のデータ例を渡して、作られるnodeをより精密にしたい

## list対応

## nodeとtypeを分ける

typeがPathを持っている形が変
