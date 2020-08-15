## egoist

- cliを手軽に
- structをサポート
- subcommandをサポート

## go flagのみのsubcommand

ちょっと試してみたけど実装するのがめんどくさそう。
このままだと行けていない感じ。

これが自然？

- https://www.digitalocean.com/community/tutorials/how-to-use-the-flag-package-in-go
- https://gobyexample.com/command-line-subcommands

いろいろやってこうなった

- https://gist.github.com/podhmo/47b865a5b31a0625dce6dcbc5a0e9fe5

### 要求を整理したい

- 手軽にサブコマンドを実装したい
- サブコマンド内で実行される処理も手軽に記述したい
- main.go自体に実際の処理は書きたくない。

詳細

- 未指定で実行された場合は存在するサブコマンドが列挙されて欲しい
- 個別のサブコマンドごとにオプションを選択したい
- (環境変数でも設定できるようになってほしい）
- サブコマンド以前にオプションを渡すことで共通オプションを定義したい
- サブコマンドと実際の処理を一つなぎに記述したい（？）
- switchでいい感じにサブコマンドを分岐したい
- 見つからなかった場合にはhelp usageを表示して欲しい

悩み

- パッケージを分けたい？
- 一つのファイルにまとめたい？
