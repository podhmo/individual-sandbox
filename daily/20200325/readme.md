## lint用のmakefile

- 全てのタスクをつなげて実行したい
- 一部のタスクだけを実行したい
- 失敗した時には、どのタスクで失敗したかが、判別可能であって欲しい
- (失敗した時には、どのタスクのどのファイルに対する処理で失敗したかが、判別可能であって欲しい)
- 失敗したタイミングですぐに止まって欲しい場合と、とりあえず可能な限り実行して欲しい場合がある

あれば嬉しいもの

- 実際には実行せずに、どの様なコマンドが実行されるかが分かって欲しい
- 特定の階層のファイル全部に対して特定のコマンドを実行し、失敗したファイルの一覧が返ってきたら、それを元にメッセージを表示して欲しい

よくあるモノ（個人的にはアンチパターンだと思うけれど）

- (可能なら、必要なコマンドを保持しているかを確認して、保持してないのであればインストールして欲しい)

### なぜシェルスクリプトではダメか

- 一部のタスクだけ実行ができない
- どこを実行しているかわかりづらい。 `echo <message> && { <script> }>` みたいな書き方はできるが


## lintとしてのgoimports

`goimports -e -l` を使う

```
  -d    display diffs instead of rewriting files
  -e    report all errors (not just the first 10 on different 
```

## make

### 実行するタスクのログ

特定のタスクの実行時にメッセージを表示して欲しい？以下くらいしか思いつかなかった。

```
00:
	$(info start $@)
```

うーん。実行の順序などを考えるとすなおにechoなどを使った方が良さそう?でも実行結果を渡せるのが便利なのだよなー

```
00:
	@echo $(shell date +%s) start $@
```

sub makeと混ざったりするとinfoは厳しいかもしれない？

### recursion

MFLAGSをつけるべきなんだろうか？

- https://www.gnu.org/software/make/manual/html_node/Options_002fRecursion.html#Options_002fRecursion


### 実行したときのpwd

```
       -w, --print-directory
            Print a message containing the working directory before and after other processing.  This may  be  useful
            for tracking down errors from complicated nests of recursive make commands.
```

こんな表示になる。

```
start foo
end foo
start bar
end bar
start boo
end boo
make[1]: Entering directory `VENV/individual-sandbox/daily/20200325/example_makefile'
make[1]: Nothing to be done for `default'.
make[1]: Leaving directory `VENV/individual-sandbox/daily/20200325/example_makefile'
make: Leaving directory `VENV/individual-sandbox/daily/20200325/example_makefile'
```

### 環境変数の上書き

```
       -e, --environment-overrides
            Give variables taken from the environment precedence over variables from makefiles.
```

## emacs

- 英語のキーボードでもタブ移動が利くようにしたい

  - ja: `C-;, C-:`
  - en: `C-;, C-'`

### elisp

特定の条件のときだけ値を挿入するみたいなことってできたっけ？

```lisp
`(1 2 3);; => (1 2 3)
`(1 2 ,@'(10) 3);; => (1 2 10 3)
`(1 2 ,@(if (= 0 (cl-random 2)) '(10) nil) 3);; => (1 2 10 3)
```

symbolで分岐するのってどうやるんだっけ？

```lisp
(require 'pcase)

(pcase 'en
  (en 100)
  (ja 200)
  (_ 0))
```

### `'` をキーバインドとして設定するのはどうするんだろう？

- https://www.gnu.org/software/emacs/manual/html_node/elisp/Keyboard-Events.html
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html#Basic-Char-Syntax
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Ctl_002dChar-Syntax.html#Ctl_002dChar-Syntax
- https://uk-ar.hatenadiary.org/entry/20120213/1329138385

```
?\C-x;; => 24

?';; => 39
?:;; => 58
(event-convert-list '(control ?x));; => 24
(event-convert-list '(control hyper ?x));; => 16777240
(event-convert-list '(control ?:))

;; 'を使うとeventとして認識されない？ 英語キーボード
```

なんか上手く動かないのでC-j n, C-j p

### defgroup

使い方とか忘れてしまっているな。。

```lisp
(defgroup my nil
  "My custom settings"
  :prefix "my:")

(defcustom my:keyboard-layout 'ja
  "My keyboard layout setting. This can be `en' or `ja'."
  :type '(choice (const en) (const ja))
  :group 'my
  )
```
