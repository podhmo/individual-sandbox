## go private fieldのupdate

もともとは引数部分にめんどくさい実装を逃した関数を書いたパッケージのテストについて考えていたのだった。
そのパッケージ単体のテストでは楽なのだけれど。そのパッケージ自体を使うコードのテストが結局だるいままになる。
という話を受けてstructにすると良さそうという感じだった。

次にそのフィールドをprivateにできるかどうかが気になっていたのだった。

## python minitask

やっていきたいのはこれ。直近の目的としてはmonogusa用のworkerとしての用途。
ただスケジュール可能なtask workerとしても使えるんじゃないかということを思ったりしていたのだった。

現状ではipcっぽいことができている。それ以上のことはできていない感じ。
workerを書くのが結局だるいという状況だった記憶。

- [../20200127/example_minitask](../20200127/example_minitask)

### 追記

とりあえず、何をやっていこうか？

- workerを手軽に書けるようにする
- jsonrpcとの癒着を引き剥がす
- どうにかしてbackground taskを手軽にやっていきたい
- batch requestとかどうする？

### 追記

- sqsでのtask_done()に近い形で試す -> 家でする
- describe的な関数を作る -> 一旦中止
- endpointがNoneでも動くようにする -> 辞める
- batch reququest, batch receive
いや、endpointを省略できるのはあんまり嬉しくないな。省略していないもの
と省略したもので意図しないところを見てしまう。

### consumerの消費の仕方

- named pipe

 - 内部で全部読み込む
 - 途中のものは保存してresume。

- sqs

  - 内部で読み込みたくない
