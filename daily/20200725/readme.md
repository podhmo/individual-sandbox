## python metashapeの拡張

- marker,guesserを宣言的にする方法を考えてみる
- もっと手軽なwalkerを (type hintsなし版？）

## python pythonを設定ファイルとして使ってみる

- 定義
- 設定

この2つは別だよなー。とはいえ混ぜて使いたい。

やれることはcueなんかを元に考えてみると良い？

- https://github.com/cuelang/cue

いろいろ試してみたけれど。cueのtype and valuesが便利すぎる。
pythonの場合は型名を書かなくちゃいけないのがめんどくさい。

あと、dataclassesは悪くないのだけど。これをpydanticにすると厳しいな。
あと、dataclasses化を忘れてしまうことがあった。これはめんどくさい

## markerを増やす。

これは最初decoratorを改造しようと思ったけれど。partialでkindをbindしてあげれば良い。
