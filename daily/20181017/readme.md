## go 値が等しいかチェック

- shallow equal
- deepEqual
- go-cmp
- pointerを無視したもの
- 異なる型

以下の様なことを考えることもある

- 一部だけを無視したい(一部だけを対象に比較したい)
- 一部の方だけ等しさを再定義したい(例えばfloatは一定の誤差を許す)
- panicや失敗の時に全体の値を見たい
- diffのみやすさ(go-diffは見やすい？)

気にしたいこと

- testライブラリは書きたくない
- diffのactual,expectedのどっちがどっちだったかわからなくなるのは不便
- 一部のstructだけを記述して済ませたい
- そもそも型を気にしたくない -> inputをjsonでやってはダメ？

### 値の比較チェック

- shallow equalではpointerが異なるとダメ（それはそう）
- deep equalでは値が同じなら異なるpointerでも大丈夫
- shallow equalではそもそも型が異なるとcompile error
- deep euqalではそもそも型が異なると全部false

### 追記

- map[string]interface{}を経由すれば楽に実装できそう(selecting)
- 何度もjsonを繰り返すのは。。(cacheしたい)
- sliceはunhashable
- https://github.com/mitchellh/hashstructure ?
- 最初のインプットをstringにしたい場合がありそう
