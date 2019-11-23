## 触りたいこと

- asgi + ちょっとしたUI
- 昼と夜の共有
- db indexのdiff
- schemalint
- metashape
- pyodideそれ自体ではなくその内部の動きを

## metashape

- 軽くで良いのでopenapi3などをparseしたい
- 個別のmarkerを用意できるようにしたい
- 個別のoptionを用意できるようにしたい
- datasetとの戦い
- designとの戦い

## schemalint

- 表示を良い感じに
- encode/filesystemのコードを覗いてもう少し綺麗に

## asgi

- 説明しやすい題材が見つかりそう
- ちょうど良いのはcrawlerとその終了判定
- そろそろまじめにUIの方も気にしたほうが良い
- 考えてみると[django channels](https://channels.readthedocs.io/en/latest/introduction.html)のドキュメントなどは読んだことがない -> まぁそうという感じだった
- テストを良い感じに扱う方法を獲得して無くない？
- どちらかと言うとJS部分をサボってたという感じ？(まぁWSだけじゃないのだけど)

### starlette

- ちょっとしたアプリのサンプルはあると便利
- 直接asgiを触る版
- Endpointを使い分ける版(https://github.com/encode/starlette/blob/7f8cd041734be3b290cdb178e99c37e1d5a19b41/docs/endpoints.md)

websocketをendpointなしでやるとこの辺りがハマるなー

https://github.com/podhmo/starlette/blob/24f73c5e0b6ca1dab532daa994ad4b62a25cfa8a/starlette/routing.py#L247-L252

## そういえばuiをテキトーに作る何かが欲しい

コレを試してみたい

- https://github.com/material-components/material-components-web-react
- [../20191120/readme.md](../20191120/readme.md)

意外と良いかもしれない。
全部を出力した後にgrid付近で調整したい。
