# monogusa

- https://github.com/podhmo/monogusa
- [昨日の続き](../../20191214/example_monogusa/readme.md)をしよう

## 思い出す

昨日は手でweb部分を書いてみる感じにしてみてとりあえずそこそこ動いた感じになったので依存を含むような処理に手を付け始めたのだった。todoアプリの雛形みたいなものがちょうど良い。

お試しにと、starlette,fastapiでの例を確認していた感じだった。手軽さを考えてdatabases辺りを使うかという気持ちになり、設定ファイルをどうやって読み込もうかなーという感じになっていたのだった。starletteもfastapiも環境変数経由だなーということとまぁ12 factorのあれ関連でソレが一般的だよなーという感じに思っていたのだった。starletteは.envを見る感じにしていたし、まぁその辺りが無難かなーと思いはじめたのだった。

一方で関数に本格的に依存したcomponentがやってくることになったのでソレへの対応をどうしようかなーと悩んだりしていたのだった（例えばdbなんかがそう）。お試しでテキトウに引数名と型を見たDI的なものを作ってみたりしたのだった。

例えば、以下の例では位置引数 (positional arguments) のXが依存したcomponent。

```python
def foo(x: X, *, y: Y, z: Z) -> None:
    ...

@fixture
def x() -> X:
    return X(...)
```

コレを変わらず `python -m monogusa.cli foo --x=xxx --y=zzz` みたいな形で呼び出したい。

とりあえずプロトタイプっぽいコードを書き散らかしてこれから組み込もうという感じになったのだった。

pytestに寄せて引数名と同名の関数を手繰り寄せて依存を構成するようにしている。あとfixtureという名前にしたけれど微妙かもしれない。teardown的な要素は必須かもしれないなーと思いつつ、そこまではできていなかった。

あとすごく軽い感じでasync defされた関数の対応もしたのか。まぁでも単体で呼べるだけの非同期処理というのはあんまり価値が無いかもしれない。

## 次にしたいこと

- 設定ファイルの読み込み
- 依存への対応
- webのコードをもう少し手軽に
- terminal以外への入出力を考えてみる (e.g. slack, discord)
- 保存先のことも本当は考えたい (e.g. sqlite (RDBMS), spreadsheet?)

## とりあえずは

設定ファイルの読み込みと依存への対応辺りをやるか。
少なくともtodo appの例が動けたらversion 0.0.0ぽい感じにできるような気はする。

この２つは地味に依存しているような気がする。設定ファイルというか.envの生成には依存のフルスキャンみたいなコマンドが欲しくなる。そういう意味では `python -m monogusa.tool` みたいなこまんどを用意しても良いかもなー。

## 理想の話

本当はこの依存のcomponent用のimportを最小にしたい。
例えばdbを利用しないコマンドなども取り扱うことになるかもなんだけれど。その場合には例えばsqlalchemyなどへのimportが一切走らないようにしたい。やろうと思えばインターフェイスを定義することでできると言えばできる。

mypy的にはtyping_extensions.Protocolが便利なんだけれど。これはisinstanceができないかもしれない。そうなるとABCのクラスを作ってregisterというのが良いのかもしれない。とはいえそれを書くのは地味に負担になってくるような気はしていて、それはもう立派なアプリケーションじゃんという感覚もある。

## :warning: hmm

やっぱり、magicalimportを使う上で同階層のmoduleは直接importしたくなるかもしれない。
任意のrelative importを許すようにしようとしたらだいぶカオスになったのだけれど。
同階層だけ許すことにしようかな。書き換えがだるい。 (out of context)

## 依存の組み込み

とりあえず依存の組み込みをしたい。考えることはなんだろう？

- 名前の調整
- `__future__ from import annotations` の対応
- component部分をコマンドの対象から除外
- markerを単に値にmarkingするだけにする？

## :bug: magicalimport

- same modules
- relative import
- without here

とりあえずやっつけた。本当は `__init__.py` なしにfake moduleを作りたい所だけれど。無理だった。

:warning: `from .di import comcomponent` と `from di import comcomponent` が違うのはちょっと面倒だなー。

### 追記

これはpythonの挙動に合わせるべき。トップレベルのモジュールならimportできるがrelative importをしようとすると失敗。という挙動が正しい。

やった。

## dependencies

それっぽい感じで実装した。とりあえずCLIは動く。
ただ、startupイベントが欲しくなったかも。

:dizzy: ほとんどmagicalimportの変更に時間を使ってしまった。。

- 本当にdiの動作チェックのためだけのもの [03](03hai/)
- 昨日のtodoっぽい機能を動かすもの [04](04notes/)
