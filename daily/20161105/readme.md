# emacs string-replaceをちょっとだけ便利に

正規表現を使って頑張ったりしていたのを止める。

例えば、以下の様な箇条書きのprefixの-を`*`に変えたい時に、`C-x r t`して`*`を選択して`-`に変えるとかできる

```
* foo
* bar
* boo
```

以下のような設定を追加

```lisp
; M-x list-packagesでmultiple-cursorsをinstall
(progn ;; multiple-cursors
  (require 'multiple-cursors)
  (progn ;; key-settings/multiple-cursors
    (add-hook 'on-after-keyboard-setup
              (lambda ()
                (define-many-keys global-map
                  '(
                    ("C-x r t" . mc/mark-all-dwim)
                    )
                  )))
    )
  )
```

とは言え、以下のものに全部箇条書き用のprefixをつけるには今まで通り正規表現が入りそう。`\(.*\)`, `- \1`

```
- foo
- bar
- boo
```

# golang importsされるものまで取り出したいかも

どうするのだっけ？

- [example_parser](./example_parser)

# golang go runでxxx.goを引数に渡す方法

以下の様なことがしたい

```
go.run main.go xxx.go
```

build対象だと勘違いされる。

## とりあえずflagパッケージ使うことにする

flag packageに利用した変数はmain()以外で触りたくないみたいな気持ちがあるので以下の様に書く

```go
package main

import "flag"

var target *string

func init(){
	target = flag.String("target", "", "target file")
}

func main(){
	flag.Parse()
	run(*target)
}

func run(fname string){
	// 実際の処理
}
```
