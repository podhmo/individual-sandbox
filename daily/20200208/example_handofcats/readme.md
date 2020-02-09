# python handofcats

https://github.com/podhmo/handofcats

## `--cont` optionを追加したい

### 適当に思ったこと

やっぱり戻り値がNone以外の関数も機能するようにしたいかも。
どういう動作が自然だろう？
monogusaのcliだけに入れるかhandofcatsにもいれるかは悩みどころ

考えてみるとpython -m handofcats <filename>:<function name>で実行しても何も表示されないというのはちょっとさみしすぎるのでhandofcats側にあって良い気がする。
そうじゃないとわざわざprintするだけの関数を書く必要がある。

型からコマンド化の利点でもあり欠点でもあるけれど。こういうようなprintするだけの関数にもフルの引数定義が必要になるのでそれは不毛だなー。
というわけでNone以外を返す関数に何らかのデフォルト動作を用意しておいた方が良さそう。

まぁ明示的にNoneを返しているものに対して"None"と表示するのは微妙だからprintではないだろう。
ミニマムな実装は`val and print(val)`かな。

print以外も指定したいよねということでオプションを取ることにはしたい。雑にcontで良いか？
エラーもキャッチしたいとなることがあったりするんだろうか？
その場合はchatbotやwebapiのことを考える感じかもなー。

### うーん

`[]` も入れたいから真面目にNoneかをチェックしよう。
codegenで扱うかどうかは悩みどころ

## `--simple` simple outputがほしいだけでは？

ついでに `setup_parser()` をmなしで使えるようにしたかったり。
defaultはcommandlineのものを使うのが正しい気がする。
あとは色んな所で渡しまくるのはだるいなー。

## ついでにもう少しmonogusaで使い回せるように

`ignore_arguments` とか `ignore_flags` とかをConfigに持たせちゃおう？
