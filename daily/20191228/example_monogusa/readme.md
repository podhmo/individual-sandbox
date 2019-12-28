# monogusa

- https://github.com/podhmo/monogusa

## どこから始めようか？

完全ではないけれど。web側のinterfaceが一段落したのだった。次はchatbot
辺りをいじりたい。一番身近なのはslackかもしれない。

なぜchatbotに対するUIが必要か？と考えるとwebAPIを直接打てる層と別の層まで範囲を拡げたいため。意外とchatというのはauthentication的にもauthorization的にもちょうど良い。

monogusaの最も卑近のユースケースはメッセージキューへのエンキューかもしれない。例えば失敗したバッチのretryみたいな作業をslack越しに提供するということなどに便利。そう考えるとworker的なものの機能も早めに用意したほうが良いかもしれない。

## slack対応

botとしての機能例えば非同期requestなどは優先度は低めで良いかもしれない。機能をとりあえずslackbot上に載せるということをやりたいかもしれない。

## discort対応

こっちもすぐに終わった。以下でcommands frameworkが使えるのかと思ったが、`--foo="bar"` みたいな値を解釈することができないみたいだった。なので結局slackと同様にやった。

- https://discordpy.readthedocs.io/en/latest/ext/commands/commands.html

## ui

今の所以下のようなuiなので統一感は持たせたい。

- cli -> eval
- web -> codegen
- chatbot.slackchat -> eval
- chatbot.discordchat -> eval

## 裏側の機能としてやったこと

- AsyncDriverを作製
- DI部分の実行にresolve_asyncを追加
- onlyデコレーターを追加
