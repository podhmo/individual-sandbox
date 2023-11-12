# example_preact

いい感じにtsxだけを書いて動作確認をする環境を作りたい。
ついでにuseState()とsignalsの違いを確認していきたい。

- [preact トレーニングモード（環境）](https://zenn.dev/podhmo/scraps/4a20ad0e96aef8#comment-969caec296cb01)

## 作業

- ok preact sandboxの作成
    - ok import()の利用
    - ok denoの利用
    - ok preact/signalsの利用

memo

- `npm:`にブラウザ側のimportmapが対応していないのでdeno.jsonにも書かないとだめ
- deno.jsonをesbuildにtsconfig.jsonの代わりとして渡すことはできそう
- preact/signalsを利用すると依存関係上preactの最新ではなくなるっぽい？ (10.18.1)

思ったこと

- denoでserverを書いてあげればversionの管理はdeno.lockに任せられるのでは？
    - freshとか使えばよくない？
- そもそもviteに移行しちゃうのはだめなの？

## examples

- hooks/signals
    - 00 useStateを使った単純なcounter
    - 01 signalsを使った単純なcounter
- state handling
    - 02 現状のstate handling, submitボタンを押したタイミングで変わって欲しい
    - 03 とりあえずhooksを使った形で書き換えたもの
    - 04 とりあえずsignalsを使った形で書き換えたもの
