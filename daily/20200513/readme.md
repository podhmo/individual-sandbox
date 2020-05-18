## 欲しい機能

結局なんだったんだろう？

- Makefileに依存を生成させたい。結局Makefileは必要。

  - include build.mkとかしておいて、build.mkを生成させれば良い
  - ファイルを監視できていないからね。。
  - 入力、出力のファイル関係を出力できると良い
  - Makefileを生成する機能もサブコマンドにできれば

    - サブコマンド自体もapp.include()でmountできれば良さそう。

- 新しくgoのファイルを作るのは面倒。prestring.text.parseを良い感じにしたものを使えれば？


## 定数生成

お試しということで、やろうとしてみたが、新規にgeneratorsを作るのがわりとだるい？
だるい。そして一度コピペでstructkitを元にやろうとしたら意外と使い回せる（それはそう）。
contextがやっぱり邪魔だなーと思うものの。

### actors

現在どのようにコード生成が行われているかというと以下の３つ

- walker
- generator
- emit function

コマンドに対してwalkerをsetする。そしてwalkerはコマンドをファイルIOに変換する。
コマンドの内部でgeneratorをsetし、generatorは内部で入力を良い感じにハンドリングしてemit functionに渡す。
- walkerは関数群を受け取り良い感じにファイルIOを作る

### walker?

walkの関数って汎化できるのでは？

- clikitは関数名 = ファイル名。 (ただし引数はoptionsをprefixに取ったもの)
- structkitは関数名 = ファイル名

この他に何があるか？

- `foo__bar` はfoo/bar ディレクトリ
- `foo__bar` はfoo/bar.go
- `foo__bar` はfoo/bar/xxx.go (xxx.goはなにか特定の名前）


## egoist,metashape

簡単な何かのために時間がかかり過ぎた。

- `__name__`無しの定義はちょっと大変

  - (トップレベルなら余裕だけど、フィールドに存在しているような間接的なものは厳しい)
  - 名前を推測すれば大丈夫？ -> enumなどは実際に登録できないとダメ？

- それとは別に、typeにmetadataを持たせたいのでは？

  - metashapeの領域かもしれないけれど。

## 今日の予定

- egoist release
- 定数生成 (multiple outputs)

  - classではなくdataclassesを見る
  - templateを利用
  - bulk action

## release

fooというvenvを作って作業をしよう

```console
$ python -m venv foo --system-site-packages
$ . foo/bin/activate
$ pip install wheel tween
```

- prestring
- metashape
- egoist

どこかで、CIが壊れていないか確認したほうが良いかも？

できた。

