## sqlite3 使いかたわすれたなー。

とりあえず `.help` しておけば良さそう。
あと細かい話をすると `.headers on` しておくと便利。

### 追記

dumpは以下の様な感じ？ .outputを使っても良いけれど。

```console
$ echo '.dump' |  sqlite3 <database name | tee <file name>.sql
```

loadは以下の様な感じ？

```
$ cat <filename>.sql | sqlite3 <database name>
```

## python pony

relationなどを調べるのにponyのドキュメントが悪くなかった。

- https://docs.ponyorm.org/relationships.html#
- https://github.com/ponyorm/pony/blob/orm/pony/orm/examples/estore.py

### 追記

謎のサービスがある。

> Create Entity-Relationship diagram with online Pony ORM ER Diagram Editor And get generated SQL!

- https://editor.ponyorm.com/

## python metashape

そろそろ使いやすさのフェイズに入っていきたい。
いろんな生成スクリプトを書き散らかすというのは全く使い安い状態ではない。

## python prestring

とりあえず複数入出力の機能を付けた
次はなんなのだろう？一旦こちらはおきで良い気がする。
