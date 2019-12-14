# monogusa

色々やっていく。基本的には関数定義以上のことをしたくない。ものぐさなので。

https://github.com/podhmo/monogusa

## as CLI

これはhandofcatsを使えばおしまい。

### 追記

dictのサポートを考える必要はありそう

### 追記

webAPIのopenapi.jsonのところで `--help` の結果を貼りたいと思うことがあった。
argparseのparserが取り出せる様になっていると嬉しいかも？

(とはいえ手元にあるのは関数定義だけなのだった)

### as webAPI

とりあえずfastAPI経由でwebAPIとして提供する方法を用意しようかな。

fastAPIに乗っかることでOpenAPI docの生成と簡易的な実行環境 (browsable API) が自動で付いてくる。

いろいろ考えた結果コマンド実行の抽象なのでresponseはなし。
ただし標準出力や標準エラー出力を返しても良いかもと思ったので全てのwebAPIで同じ構造にすると良さそう。とりあえずcontextlib.redirect_xxx()系でお茶を濁す。

しかし、せっかくのasgiなのに、そのまま実行してしまう形態なのは、もったいないような気もする。websocket経由のendpointも公開したいかもしれない（ところでこれはOAS (= OpenAPI spec) と相性があまり良くないような気がする）。
あと、場合によってはsubprocessとしてspawnしたほうがprocess自身は穏やかになるかもしれない。

### 追記

grpc化したりするのは付帯作業程度に考えれば良いか。手軽さという上ではJSONRPCの方が優先度高いかもしれない。あくまでものぐさなので。。

### 追記

個人的にはweb経由でのUIを一切見たくないことがある。openapi.jsonを`--show-doc`みたいなオプションを付けたら出力するようにしてしまおう。

### 追記

あと手元でuvicornを経由して実行するのがだるいかも。直接実行できるようにしてしまいたい。subprocessとか使うくらいならexec*系の関数を呼んでしまうのが良いかもしれない。

## web runtime

とりあえずruntimeで使いそうなhelper関数はmonogusa.webに入れちゃうか。。
どこかのタイミングで実行時以外のannotationが出たタイミングでmonogusa.web.runtimeなどに移動したくなるかもしれない。

コレを使って手書きした01を02として書き直すか。ソレが終わったら00から02を生成するか実行できるようにする。

（まだしばらくはConfigurator的なものはなくて良さそう。とりあえずデフォルト実装を埋める感じ）

### 追記

それなりに整理した。mypyも通る様になった。
ついでにgithub actionsを導入してみたけれど。まじめに設定はしていない。
defaultのままだとけっこうymlの方にやることを書いてしまう感じなのでアレかもしれない。

## 次の方針

流石にhelloやbyeの表示だけだとあんまり意味が無いかもしれない。
どうしようかな。。

- webの方にtodo app的なexamplesを作る
- webの方のpydantic modelの生成部分を作る (引数のsignatureからあれこれする)
- slack,discordあたりと繋げる？

## とりあえずCRUDを作るか

- https://www.encode.io/databases/#databases

テキトウにstarletteのdatabaseのやつを見てみたらこれがそのままtodo appの最もミニマムバージョンじゃん。コレで良さそう。

- https://www.starlette.io/database/

### 追記

fastapiでも作るか。。

概ね存在していた。。

- https://fastapi.tiangolo.com/tutorial/async-sql-databases/

transactionsのことは気にしないとかも

- https://www.encode.io/databases/connections_and_transactions/

色々似たようなものを作らないとダメそうな感じはあるかも?

