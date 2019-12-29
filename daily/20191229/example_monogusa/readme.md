# monogusa

- https://github.com/podhmo/monogusa

## 過去

```
$ git ls-files ../../ | xargs dirname | grep -oP ".*example_monogusa$" | sort -u | tac | sed 's@.*@- [\0](\0/readme.md)@g'
```

- [../../20191228/example_monogusa](../../20191228/example_monogusa/readme.md)
- [../../20191225/example_monogusa](../../20191225/example_monogusa/readme.md)
- [../../20191222/example_monogusa](../../20191222/example_monogusa/readme.md)
- [../../20191221/example_monogusa](../../20191221/example_monogusa/readme.md)
- [../../20191218/example_monogusa](../../20191218/example_monogusa/readme.md)
- [../../20191217/example_monogusa](../../20191217/example_monogusa/readme.md)
- [../../20191216/example_monogusa](../../20191216/example_monogusa/readme.md)
- [../../20191215/example_monogusa](../../20191215/example_monogusa/readme.md)
- [../../20191214/example_monogusa](../../20191214/example_monogusa/readme.md)
- [../../20191211/example_monogusa](../../20191211/example_monogusa/readme.md)

## 最近のこと

昨日は、discordとslackとを繋げたのだった。そして大きな発見がありchatbotを作り込みたいわけではないという気持ちの意味を言語化できたのだった。認証と認可の緩和をしたいという話の模様。なので特定の環境からの接続を用意するものであって、特定の環境下での生活をサポートするというようなchatbotとは異なると言うことが分かってきた。

なのでchatbotで行える全ての機能を満たす必要は無い感じ。一方でそこで呼び出される関数がchatbotからも呼びやすい形になっている必要はあるかもしれない（monogusaのために変な色は付いてほしくないというような意味）。

これによって昨日はslackbotというライブラリの実装に対してわりと不平不満を言ってしまっていたが、それを解消する優先度はだいぶ下がった。bot自体は同期的でもそこまで気にしなくても良いかもしれない。ここではコストの高い計算をする予定がないので。

あとmonogusaが一番輝くのはメッセージキューへのenqueueあたりと言うことが分かってきた。ただしここでも機能的にはキューをエミュレートした存在と良い感じに繋がっていて欲しいという感覚はある。

## 名前を変えたい

slack用のintegrationのモジュールをmonogusa.chatbot.slackchatにしていたけれど。↑に伴って名前を変えたほうが良いかも。たぶんslackcliとかの方が正しい。

今日はそれができたら記事などを書く？（あまり気が乗らない）。

## module scopeのcomponent

開発という意味ではmodule scopeのDIを追加したいかもしれない (module scopeというのはpytestのfixture的な見立てで)。まぁこの辺りはonceというデコレーターを用意するくらいで良いかも。teardownなしで逃げているけれど、そのうちまじめに実装しなければ行けないかもしれない。

## まじめにtask queue的なものを考える

background taskのもうちょっと良い感じにした版。やっぱり手軽な実行という意味ではsqsやgcpのpub/subみたいな設定が必要なものがいきなり来ては良くないし。elasticmqとかredis経由のものとかdockerが使いたくなるようなものもあっても良いけれど最初の機能ではなさそう。

ちょうど良い感じのinmemory databaseがあると良いのだけれど。
