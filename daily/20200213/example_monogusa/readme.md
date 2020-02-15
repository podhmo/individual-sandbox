# monogusa

## subscribe functionはCLIの対象外にしたい

今は載っちゃうからね。。

## そのcontextに沿ったdefaultのcomponentを定義したい

例えば以下の様な状況

- CLIアプリとして起動したときの `message.reply()`
- slackbotとして起動したときの `message.reply()`

messageにreplyを付けるのかそれともなにか特殊なcomponentをmessageが共有するのかは、別途気にする必要があるかもしれない。とりあえず、contextによって実装を変えたい。

これを考えるときに、引数名ではなく型によるDIが欲しくなるのではないか？

