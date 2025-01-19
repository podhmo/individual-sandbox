# react-flowあたりでいい感じに依存を表示したい

なんとなくreact-markdownを覗いてみたりして過度にパッケージが分かれすぎているんじゃないか？と思ったりした。
（実際のところは依存関係を追う事ができなくて辛い。ワーキングメモリーの限界に達したみたいな感じ）

そんなわけで必要な依存関係をいい感じに可視化して📝をしてみたいと思ったりした。
とはいえ、すべての依存関係をそのまま表示してもあんまり嬉しくない。

- あるパッケージの依存を表示
- 必要そうな依存をマーク
- マークしていた関係をいい感じの出力でクリップボードにコピー

というようなことができれば良いと思ったりした（そういえばnxに依存関係をそれっぽく表示するuiが存在していた（使い物にならなかった））。

## react-flow

https://reactflow.dev/

なんかテキトーに表示を試してみたい。全然理解をしていないのだけれどグラフのノードは移動できなくても良いかも。

https://reactflow.dev/learn

これで実行して済ませられるっぽいんだけれどあんまりやりたくないな。。

```console
$ npx degit xyflow/vite-react-flow-template app-name
# deno run -A npm:degit xyflow/vite-react-flow-template app-name
```

https://github.com/Rich-Harris/degit


これをそのまま実行すると手軽に試せるけれどgistに載せるように分解しなければ行けないのが面倒かも？

あとviteから読み込む場合にはcssをimportできるけれど、esbuildでそれは無理なようだ（pluginが必要）。

## 