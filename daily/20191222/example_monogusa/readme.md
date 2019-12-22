# monogusa

- https://github.com/podhmo/monogusa

## 最近のmonogusa

```
$ git ls-files ../../ | grep -ioP ".*example_monogusa/readme.md$" | sort -u | sed 's/.*/- [\0](\0)/g' | tac 
```

- [../../20191221/example_monogusa/readme.md](../../20191221/example_monogusa/readme.md)
- [../../20191218/example_monogusa/readme.md](../../20191218/example_monogusa/readme.md)
- [../../20191217/example_monogusa/readme.md](../../20191217/example_monogusa/readme.md)
- [../../20191215/example_monogusa/readme.md](../../20191215/example_monogusa/readme.md)
- [../../20191214/example_monogusa/readme.md](../../20191214/example_monogusa/readme.md)

## なにをしよう？

とりあえず昨日の段階でweb側のコード生成ができたのだった。importの部分に難があって本当はprojectの位置のようなものを固定したいかもと思ったりした。

それはそれとして今日は少しテスト辺りを整備しつつまた使用感を試す記事を書いてみても良いかもしれない。

## テストを綺麗に

少なくともcliとwebが混在していると辛いかも。
あと回帰テスト的なe2eとドキュメンテーションとしてのテストを分けたほうが良いかもしれない。

テストといいつつMakefileによるe2eなのだけど。

## そういえばDIの例(test)を用意していなかった

テキトーに追加しよう

## エラーハンドリングをやっていなかった

とりあえずstderrに出力するようにしないと。
ステータスコードのようなものをresponseで返すようにしても良いんだろうか？

エラーハンドリングはデバッグのときだけフルのスタックトレースを返すとかの方が良いんだろうか？セキュリティ的なことを考えると。まぁ良いや全部そのままstderrとして返そう。

でも200以外のstatusコードで返したいよね。。。
そう考えるとresponse_modelにstatus_codeを渡せたっけ？

- https://fastapi.tiangolo.com/tutorial/additional-responses/

意外と大変そうだ。とりあえずstatus codeを別途新生してそれをシェルの終了ステータスのように使おう。

### 追記

dataclassesとしてのstateとresponse modelのCommandOutputって分ける必要あるんだっけ？
まぁ意味合いとしては別物か。


## 考えてみるとopenapi docを直接見るの辛くない？

routes的なオプションを追加したくなるかも

## 思ったこと

おもったことをつらつらと書いていく。

- 現在のwebのresponseの構造が良くない
- かんたんなapi clientの実行でapi examplesを作りたいかもしれない
- そろそろslackあたりとの連携の機能を考えると良いかもしれない
- go_exec的な機能が欲しくなった

### 現在のwebのresponseの構造が良くない

これはけっこうそのままの話で現状はwebでcommandを公開した時に、responseのfieldとしてstdoutとstderrが分かれているのだけれど、これは例えば、stderrに出力されるlogger出力が、stdout上でどの辺りに位置するかという情報が喪われてしまう。これは意外と不便そうという話。

### かんたんなapi clientの実行でapi examplesを作りたいかもしれない

これはまぁmonogusaのexamples/e2e/webの中でシェル上での確認がめんどうという話にも関連があるのだけれど。それ以外にもかんたんな実行例がreadmeとして提供されていると便利だなーと思ったりした。それ用のちょっとしたclientを持っていても良いのかもしれないと思ったと言う話。

webとして見做すかasgiとして見做すかと言うのは難しい話だけれど。最初はasgi用にして必要ならweb用のなにかとしてwrapしてあげれば良いんじゃないかと思ったりしている。

### そろそろslackあたりとの連携の機能を考えると良いかもしれない

そろそろcli,web以外にも手を伸ばしていきたくなってきた。一番日常的に使うのはslackなのでこの辺りの連携が手軽になっていると嬉しい。botを導入する必要が出てくるのかどうか辺りが分かっていないけれど。

基本的にはhelpが生成されてほしいかも。 `/<command>` か `/<bot name> <command>` という形で提供される様になる感じ。CLIがそのまま使えれば良いんじゃないかな。

少なくとも以下は達成されて欲しい

- subprocessのspawnとawaitはしっかり効いて欲しい
- 基本的にはsubprocessで実行して自身は詰まらせない方が良い？(場合による)
- .env辺りで設定ができて欲しい

### go_exec的な機能が欲しくなった

考えてみるとちょっとしたtask runnerのwrapperとして使えても良いのかもしれないと思った。何かのprocessとしてwrapされているのではなくそれそのものとして実行されるというような機能が作れても良いのかもしれない。

これはどちらかと言うと関数の中をどう書くかみたいな話。

## そういえば

crud的な位置づけのモジュールのところに安易にmain()を生やすみたいなことをしたり、componentの定義を`if __name__ == "__main__"` の中に入れておくというような方法はありかもしれない。

