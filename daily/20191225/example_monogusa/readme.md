# monogusa

https://github.com/podhmo/monogusa

## hmm

とりあえずCLIの方もコードを出力できるようにしてみようかな。
そしてwebの方もtmpfile的なものに出力して実行できるようにしてみると良いかも？

### 追記

意外とこれが大掛かりだということが分かった。importして上手く使えるような体制になっていなかった。なるほど。

あと、runtimeに寄せるものはなにかとかも。

## blog

書こう書こうと想っていたweb部分に関する記事を書いた。わりとだるい。

monogusaとか作っている理由はある意味時間の貯金がしたいみたいな感じなのだよなー。ある物事をやろうとおもったとして取れる時間が数時間しかない。やりたいことは２日くらい掛かる。みたいなことが重なりがち。

一方でそのやりたいことはメンテが必要なことでそれをずっと維持したいわけではないので、それそのものを２日だけ無理やり時間を作って行ってもビミョーみたいな。

## 次どのあたりしよう？

分かっているrefine的な作業

- CLIもコード生成
- コード生成用のinterfaceを整える
- responseの形状を整える
- 非同期での依存解決を提供
- dbを使ったようなもう少しまじめなexampleを書く
- CLI的なUIを良い感じに扱う方法の整理

次にすすむならどのあたりだろう？

- chatbot
- startup,teardownイベント
- taskキューの実行

chatbotを経由してwebsocketとの付き合いをどうするかを考える価値はあると想っているのだよなー。

あと迷わない設定という意味ではmetashapeと繋げたいな。

chatbotやるか。

## slack?

まぁslackで良いか。テキトーにdotenv経由で見れる感じにして作業をしよう。
その前にslack関連で良いライブラリがでていないか調べよう。

### 追記

特になかった。

## 今日はちょっとした調整の日にしようかな

- ignore marker
- 非同期でDI
- codegenをmoduleに移動
- responseの形状変更
- CLIでcodegen?
- doctorみたいなコマンド作る？scan?どう扱われているかを知るみたいな

### 追記

この辺やった

- ignore marker
- 非同期でDI
- responseの形状変更

