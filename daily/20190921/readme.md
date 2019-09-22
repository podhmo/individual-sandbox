## emacs flycheck 使いかたを忘れてしまった

[../20190814/readme.md](../20190814/readme.md)

### そう言えば

flycheck-set-checker-executable を使う様に書き換えた方が良いかも

あとgoが重いのはflycheck-disable-checkerを付けてあげれば良い。

### flycheckの設定の確認

- M-x flycheck-verify-setup
- M-x flycheck-verify-checker
- M-x flycheck-list-errors (C-c ! l)

### log level

ないかも

## python schemalint

- cueができて不要になってくれたら嬉しいなー。

## go DI

- constructor injection的な形が好き
- DIを本気で考える必要があるのは依存の依存が共有されたとき
- 前提として設定による分岐と文脈による分岐が把握できている必要がある

### 対応予定の方針

- sharedなパッケージを作りここでコンポーネントを作成
- registry的なオブジェクトに登録する
- ただし、そのままだと全ての依存が全てのバイナリの依存になるので直接は使わないようにbuild

### 制約

- main.goひとつだけを指定してgo runで動かせることは死守したい
- main.go内のコードが太るのは悪
- ビルドタグで分岐という形で持っていくと楽だけれどビルドシステム必須ということはなるべく避けたい

