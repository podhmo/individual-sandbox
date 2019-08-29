## go handy ちょっとしたtesting library

### motivation

- testify.assert, testify.requireは大げさで種類が多すぎる
- でも元気が無いときにはtestingだけだとツライ

### concept

- assertion libraryってほとんどerror checkとして扱えるんじゃないの？

その他ちょっとした事柄

- testingでのif文を書きたくない
- testifyの書き方だとactual valueとexpected valueの区別が面倒
- `c, err := do()` 的な多値でerrorを返すAPIを手軽に書けるようにしたい

### 実装の概要的なもの

:warning: これは暫定的なもの

- Assert(), Require()がt.Error(), t.Fatal()に対応
- 比較はEqual(),DeepEqual(),JSONEqual()
- 否定の関数を別途用意するのはあんまり良い感じではなかった。

  - おかげでNotEqual(), NotDeepEqual(), NotJSONEqual()という関数が

- 比較先のexpected valueの保持はExpect()?
- 多値(エラー)対応の名前は例えばEqualAndNoError()?

概念的なにはAssert(), Require()は単にerrorを受け取るだけの関数。

最もシンプルな記述ではこういう形に。

```go
handy.Assert(t, handy.Equal(<got>).Expected(<want>))
```


### 追記

やってみた結果

https://github.com/podhmo/handy

なんか色々と問題があるかも？

- 名前をtestifyを参考にした感じにしたら間延びしてしまっている
- errorの扱いを良い感じにしたい
- もうちょっとassertion errorをメッセージを豪華にしたい
- 何かメッセージを埋め込みたい。
- 途中でエラーになったときのエラーメッセージの表示

間延びしているような気がする。

```go
// *testing.T
if got := add(10, 20); got != 30 {
	t.Errorf("want "%d", but got %d", got)
}

// handy
handy.Assert(t, handy.Equal(add(10,20)).Expected(30))

// testify/assert
assert.Exactly(t, 30, add(10,20))
```

assertionに失敗したときのreportingの内容。testifyに併せに行く意味あるんだろうか？

```
--- FAIL: TestHandy (0.00s)
    handy_test.go:11: 
        Where: StrictEqual
        	actual  : 10
        	expected: 20
--- FAIL: TestTestify (0.00s)
    testify_test.go:11: 
        	Error Trace:	testify_test.go:11
        	Error:      	Not equal: 
        	            	expected: 10
        	            	actual  : 20
        	Test:       	TestTestify
FAIL
exit status 1
FAIL	m	0.005s
```

### 追記

名前間違えていた

- NotJSONEqual()の方がJSONNotEqual()などより良さそう？
- Except()ではなくExpected()。これは完全にtypoしてた。
- Report()よりMessage()が良い気がする。

細かな関数名の調整

### 追記

reportingをもう少しtestingをそのまま使うのにあわせて良いんじゃ。
あと、`DeepEqual()`とか表記されても別に嬉しくない。

一応以下の様な形で整形できるようにしたけれど。もう少し良い方法があるかもしれない。

- WithMessage()
- WithDescriptionFunction()

### 追記

どうしてもWithMessge()は無理だった。

```go
handy.Assert(t, handy.Equal(<got>).Expected(<want>).Describe("context"))
```

あー、responseなどをここでで表示したいのか。

### 追記

- ActualとExpectedの位置を逆にしたほうが良い。

理由は２つある。

- error mesageのときのexpected, actualの順序と揃えられること
- errorをハンドリングする処理をメソッド１つですませられること。

後者は現在の状態に対応させようとすると関数が倍になる。

- Equal,DeepEqual,JSONEqual
- not (Equal,DeepEqual,JSONEqual)

これがさらにError対応で倍に。

一方Actual側に置くなら１つメソッドを追加するだけで大丈夫。

### 追記

functional optionsを捨てたほうが良い。
あんまり活躍していない。単にreporterのメソッドにしてあげれば良いだけ。

### 追記

２つとも目論見は完全に上手く言った。素晴らしい。 :tada:

### 追記

名前はともかくあるきのうがなぜ必要か？

- Describe() -- Equal, JSONEqualだけでは意味がわからない。何かメッセージを付加したい。
- Epilog() -- stausが200以外だった時にresponseの中身を確認したい。ただし失敗しないときにはresponse.Bodyをio.Readerとして消費してほしくない。
- ActualWithNoError() -- `c, err := count()` みたいな関数用

### 追記

- Epilog()を消す

この辺り気にしても良いのでは？(testify側からではなくtesting側)

- https://qiita.com/Jxck_/items/8717a5982547cfa54ebc
- https://golang.org/src/net/http/serve_test.go#L224

### 追記

メソッド名を変える。

- Require() -> Must()
- Assert() -> Should()
- Message() -> Log()

### 追記

readmeもそれっぽい感じにするか。タグを打った。完成 :tada:

https://github.com/podhmo/handy

### 追記

- パッケージ名を変えるとしっくりくる handy -> noerr
- argsを直接取る
- WithErrorでは？

## vscode

```
$ brew cask install visual-studio-code
```

- https://qiita.com/sensuikan1973/items/74cf5383c02dbcd82234

  - https://marketplace.visualstudio.com/items?itemName=CoenraadS.bracket-pair-colorizer-2
  - https://marketplace.visualstudio.com/items?itemName=robertohuertasm.vscode-icons
  - https://marketplace.visualstudio.com/items?itemName=wayou.vscode-todo-highlight
  - https://marketplace.visualstudio.com/items?itemName=ionutvmi.path-autocomplete
  - https://marketplace.visualstudio.com/items?itemName=shardulm94.trailing-spaces

- vscode-go

  - go-importsにするのどうするんだろう？
  - https://github.com/golang/go/wiki/gopls#vscode

- emacsのkeybindで

  - https://ganaware.hatenadiary.jp/entry/2019/01/02/235708
  - https://marketplace.visualstudio.com/items?itemName=tuttieee.emacs-mcx

### 操作

- Ctrl + shift + p
- option + x -- show all commands

### commands

- open settings (JSON) -- settings.json
- toggle autoSave -- ?

### hmm

- go-outline ってなに？

###

- https://code.visualstudio.com/docs?start=true
- https://code.visualstudio.com/docs/getstarted/keybindings#_keyboard-shortcuts-reference
