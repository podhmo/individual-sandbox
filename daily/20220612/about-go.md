## goでいつ何を使うか？

- global変数とinit()

  - toolやmiddlewareでは活躍。それ以外では避けるべき。micro serviceを徹底していた場合にはsmall web applicationでも大丈夫かもしれない。
  - 環境変数等によって処理をちょっとだけ分岐したり、デフォルト値を変更したり程度はできる
  - 大きめのapplicationでは必ず負債になる

- 高階関数

  - 一番素朴な汎化用の機能。インターフェイスを実装するためのstructとメソッドのペアを作ることなく利用可能

- struct

  - 値をあるスコープから移動した箇所（別のスコープ）でも保持して利用したい場合に使う。

- interface

  - 特定の用途により汎化したもの。goの場合は実装を定義する側ではなく利用する側が作成する。
  - 最も素朴な例は高階関数。しかしinterfaceはシンプルではないので都度メソッドを実装する必要が出てくる。
  - compositeパターンのように同じ形のinterfaceをつなげることで拡張性を持たせることもある(e.g. net.http.RoundTripper)

- generics

  - 実装を共有した差分プログラミングのときに使う。複数の型を利用者側からみて同じ形状の型とみなす。
  - interfaceとは異なり個別のメソッドの定義をすることなく、同じ実装を通ることで、汎化させられる。
  - (引数に関数を取る関数、引数にinterfaceを取る関数、引数に型変数を取る関数を比較する)

- reflect

  - ある特定のstructの持つフィールド群に対して問い合わせる形でアクセスできる (e.g. loop)
  - 基本的には遅い。実行時にエラーを起こすことになるのでコア部分では使いたくない
  - (ここには書いていないがいつmapやsliceだけで済ませるかみたいな話もあるべきでは？)

- コード生成 (go generate)

  - 利用するために1ステップ必ず必要になる (generate -> build -> run (runtime))
  - generics, interfaceではできなかったstructの持つフィールド群に対する問い合わせの結果をコードに落とせる
  - 静的解析でやる場合とreflectでやる場合がある (静的解析でやる場合にはメタデータをマークする方法がネックになる)。
  - 入力にgo以外のコードを取ることもある (e.g. sqlc)
- embed

  - 実行時にアセット（e.g. 画像やテキストテンプレート)を保持したままバイナリ１つにできる
  - 特定のファイルを特定の箇所に置くなど気にすることなく、ビルドがバイナリを置くだけのママの状態を維持できる

- クロージャ

  - 状態を閉じ込めたい場合に使う。意図せずchannelとgoroutineと組み合わせるときに必要となる場合がある
  - アプリケーションで必要になったら黄色信号。

補足事項

- メソッドの型と同じ型を持つ関数型 (e.g. net/http.HandlerFunc)
- embedded
- tags (build時の分岐)

## 思ったこと

↑に書かれている内容をまとめられるんじゃないか？と思った。思考の整理として。
幾つか用途(usecase)によって優先順位は変わってくるような気もしている。
ただし、一定のガイドラインは共通しているような気もする。どういう状況では **役に立たないか？** は共通しているはず。

XXXでは無理だがYYYでは可能になるただしZZZが必要になる。みたいな形で記述できるときれいなんだろうか？


用途

- library
- tool
- middleware
- small web application
- application library
- large web application

用途の違いの例

- toolはコマンドとして別個にインストールして利用可能化どうか？(pecoとかfzfみたいなやつはtool)
- middlewareは何か特定の利用者との接続を想定しているもの(e.g. reverse proxy)
- libraryとapplication libraryの違いは、applicationに関する情報を知っているかどうか？例えば、そのままパッケージとして公開できるなら必ずlibrary。逆にmain()などで特定の環境用の設定（e.g. monitoring)用のショートハンドを保持するようなものはapplication library
- large web applicationはmodular monolith, あるいはDIが欲しくなった状態。あるいはmodelなどの機能名のパッケージ名では無理になったもののこと
- small web applicationはいわゆる世で見つかるようなgoのapplication例。バッチなどはせいぜい数個

細かい話

- context
- defer
- goroutine

不足している事項

- error handling
- logging
- metrics
- cli option parser

コレを入れると一気に増える

- テストの書き方
- mockを使うか、fakeを使うか、インフラを含めたfakeを使うか

## お気持ち

- 用途によって良さそうな形状は異なるのではないか？（指針や方針として機能する部分が異なるのでは？）
- 「どういう機能が存在するか？」という情報と「どのタイミングで使い分けるか？」は異なり、そろそろ後者を自信を持ってまとめて忘れられるのではないか？
- goらしさ？とかclean architecture ( + DDD ) とかの目線を一旦忘れてみると良いのではないか？ (良いものは良い一方で悪いものは悪い)