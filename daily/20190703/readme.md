## anytables python

- markdown
- csv
- spreadsheet

### spreadsheet tables

- urlから良い感じに取り出せたっけ？
- sheetってアクセスできたっけ？

## emacs 重たくなる原因なんだったっけ？

- 忘れた。全ての操作に対するhookを入れられたはずでその付近だった記憶。
- buffer-modifiedだっけ？

## emacs lsp-modeを試す

```
M-x package-install lsp-mode
M-x package-install lsp-ui
M-x package-install company-lsp
```

## とりあえずgoで

```
M-x package-install lsp-go
```

https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-go.el

使っても良い気がする。

しかし package-installで入る lsp-goはすごく古そう。

設定自体はここにやり方が書いてあるな。

https://github.com/golang/go/wiki/gopls#emacs

### lsp

- package-installで入るlsp-goは古い
- C-x C-sでgoimportsが効かない
- defaultではgoimportsが使われなさそう
- error行の内容はC-h .しないと見えない?

### gopls

```
$ go get -u -v golang.org/x/tools/cmd/gopls
```

- logファイルの出力
- formatterの変更

### lsp-ui

- lsp-ui-sidelineはふつうにじゃまなので要らなそう
- uiのdocumentの表示が左によりすぎている。topよりはbottomの方が良さそう。

### hmm

```
Warning (emacs): Yasnippet is not present but ‘lsp-enable-snippet’ is set to ‘t’. You must either install yasnippet or disable snippet support.
```

yasnippetは昔使って不要だったのだけれど。使ってみるか。やっぱり要らなそう。

https://github.com/emacs-lsp/lsp-mode/issues/903
